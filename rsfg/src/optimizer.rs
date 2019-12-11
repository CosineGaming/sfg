/// optimization pass over low level bytecode
use crate::llr::{*, Instruction::*};

static OPTIMIZE: bool = true;
static INLINE_THRESHOLD: isize = 64;

/// general principles:
/// MORE INSTRUCTIONS IS WORSE almost all the time (reading from bytecode is slow)
/// within instructions, the stack is FAST (caching), much faster than Load/Store
/// method calls are worse yet

/// should never error, because given a valid program bytecode, and outputs
/// one too
pub fn optimize_llr(mut llr: LLR, mut next_label: usize) -> LLR {
    if !OPTIMIZE {
        return llr;
    }
    // TODO: remove unused functions
    // problem being, fns are all public so "unused" is hard to describe (no main)
    // options: declare main as entry point, add public tag
    // or better just do it from the std library when we have imports
    inline_all(&mut llr.fns, &mut next_label);
    for func in &mut llr.fns {
        func.instructions = peephole(&mut func.instructions);
    }
    debug!("{}", llr);
    llr
}

/// row = caller
/// col = callee
type Adj = Vec<Vec<usize>>;

fn column(adj: &mut Adj, col: usize) -> Vec<&mut usize> {
    adj.iter_mut().map(|row| &mut row[col]).collect()
}
fn zero_col(adj: &mut Adj, col: usize) {
    for i in column(adj, col) {
        *i = 0;
    }
}
fn col_sum(adj: &mut Adj, col: usize) -> usize {
    column(adj, col).into_iter().map(|i| *i).sum()
}

/// returns which fns were successfully inlined,
fn inline_and_deps(fns: &mut Vec<Fn>, adj: &mut Adj, fn_i: usize, next_label: &mut usize) {
    if adj[fn_i].iter().sum::<usize>() != 0 {
        // we still call people that we want to inline, nothing we can do
        return;
    }
    // no one wants us inlined, optimization to exit early
    if col_sum(adj, fn_i) == 0 {
        return;
    }
    // NB fn_i is callee_i
    let callee = fns[fn_i].clone();
    debug!("inlining function {}", callee.signature.name);
    for (caller_i, caller) in fns.iter_mut().enumerate() {
        // performance check only ("are there instructions to change")
        if adj[caller_i][fn_i] != 0 {
            let mut count_inlined = 0;
            caller.instructions = caller.instructions.iter_mut().flat_map(|inst| {
                if FnCall(fn_i as u16) == *inst {
                    count_inlined += 1;
                    inline_fn(callee.clone(), next_label)
                } else {
                    vec![*inst]
                }
            }).collect();
            // make sure we got them all i guess
            debug_assert_eq!(count_inlined, adj[caller_i][fn_i]);
            // now they no longer depend on us
            adj[caller_i][fn_i] = 0;
        }
    }
    // now that we've removed one of their dependencies,
    // maybe they're ready to inline as well?
    for caller_i in 0..fns.len() {
        if adj[caller_i][fn_i] != 0 {
            inline_and_deps(fns, adj, caller_i, next_label);
        }
    }
}
fn inline_fn(func: Fn, next_label: &mut usize) -> Vec<Instruction> {
    let label = *next_label; // CHECK something cleaner than passing all this around
    *next_label += 1;
    let mut mapped: Vec<Instruction> = func.instructions.into_iter().map(|i| {
        // since we'll have this inline function and n others, we need
        // to change the labels somehow
        // our approach: use the label, but add an incremented label
        // note we only increment per inline, NOT per mark because then they wouldn't match
        // we let adding the original label do the job of disambiguating within the fn
        // we could get a duplicate with n(4) + lbl(6), n(3) + lbl(7) for example
        // so we multiply the label part with a constant that makes it magnitudinally different
        // CHECK this isn't exactly clean....
        static LABEL_DISAM: usize = 0x1000;
        match i {
            // for the return jump, we simply use the per-inline label,
            // as we know it's not a conflict and it doesn't conflict with the
            // modified labels
            Return => Jump(label),
            LabelMark(n) => LabelMark(n + label * LABEL_DISAM),
            JumpZero(n) => JumpZero(n + label * LABEL_DISAM),
            Jump(n) => Jump(n + label * LABEL_DISAM),
            _ => i,
        }
    }).collect();
    // the return has to jump to end because we don't have a call stack to do it
    mapped.push(LabelMark(label));
    debug!("mapped {:?}", mapped);
    mapped
}

fn inline_all(fns: &mut Vec<Fn>, next_label: &mut usize) {
    // form adjacency matrix of who calls whom
    let adj_row = vec![0; fns.len()];
    let mut fn_adj = vec![adj_row; fns.len()];
    for (caller_i, caller) in fns.iter().enumerate() {
        for inst in &caller.instructions {
            if let FnCall(callee_i) = inst {
                fn_adj[caller_i][*callee_i as usize] += 1;
            }
        }
    }
    // adjust adjacency matrix to only include those that will be inlined
    for (func_i, func) in fns.iter().enumerate() {
        let recurses = fn_adj[func_i][func_i] != 0;
        let fn_size = func.instructions.len();
        // add up all counts referencing this
        let fn_uses: usize = col_sum(&mut fn_adj, func_i);
        // TODO: fn_uses is minus 1 for one canonical representation once we can delete fns
        // fn_size is minus 2 for FnCall / Return
        let size_impact = fn_uses as isize * (fn_size as isize - 2);
        // if we DON'T want to inline, remove it from the adj of all callERS
        if size_impact >= INLINE_THRESHOLD || recurses {
            zero_col(&mut fn_adj, func_i);
        }
    }
    loop {
        for i in 0..fns.len() {
            inline_and_deps(fns, &mut fn_adj, i, next_label);
        }
        if fn_adj.iter().map(|row| -> usize { row.iter().sum() }).sum::<usize>() == 0 {
            // no more functions expecting functions to be inlined to them! (we win)
            break;
        } else {
            // we made it all the way through and were never able to resolve some.
            // to make a continued effort to inline as much as possible + wanted,
            // we decide not to inline an arbitrary (first wanted) function
            // and then try the whole process over again
            for i in 0..fns.len() {
                if col_sum(&mut fn_adj, i) != 0 {
                    debug!("cannot resolve an inline graph, cancelling {}", i);
                    zero_col(&mut fn_adj, i);
                    break;
                }
            }
        }
    }
    // TODO figure out how to delete now-inlined functions when we mark pubs
    // until then we'll just have dead code i guess
}

fn peephole(insts: &mut Vec<Instruction>) -> Vec<Instruction> {
    insts.reverse();
    let rinsts = insts;
    let mut out = vec![];
    while let Some(what) = rinsts.last() {
        match what {
            // Optimize multiple nots
            Not => {
                let mut count = 0;
                while let Some(Not) = rinsts.last() {
                    count += 1;
                    rinsts.pop();
                }
                // only odd nots make it
                if count % 2 == 1 {
                    out.push(Not);
                }
            }
            Push(_) => {
                let push = rinsts.pop().unwrap();
                let push_lit = match push {
                    Push(what) => what,
                    _ => unreachable!(),
                };
                match rinsts.last() {
                    // Avoid stack op for Push,Store
                    Some(Decl) => {
                        rinsts.pop();
                        match rinsts.last() {
                            Some(Load(0)) => {
                                // reverse state to just the push on so we
                                // can do the Decl,Load opt
                                out.push(push);
                                // put back the decl (CHECK: smelly?)
                                rinsts.push(Decl);
                            }
                            // simple DeclLit
                            _ => {
                                out.push(DeclLit(push_lit));
                            }
                        }
                    }
                    Some(&Store(ri)) => {
                        rinsts.pop();
                        match rinsts.last() {
                            Some(&Load(n)) if n == ri => {
                                // reverse state to just the push on so we
                                // can do the Store,Load opt
                                out.push(push);
                                // put back the store (CHECK: smelly? + duplicated with above)
                                rinsts.push(Store(ri));
                            }
                            // simple StoreLit
                            _ => out.push(StoreLit(ri, push_lit)),
                        }
                    }
                    // a^0 => a
                    Some(Xor) if push_lit == 0 => {
                        rinsts.pop();
                    }
                    // JumpZero 0 => Jump
                    Some(JumpZero(l)) if push_lit == 0 => {
                        out.push(Jump(*l));
                        rinsts.pop();
                    }
                    _ => out.push(push),
                }
            }
            Decl => {
                let decl = rinsts.pop().unwrap();
                match rinsts.last() {
                    // Decl,Load(0) => Dup,Decl
                    // avoids count byte, load op
                    Some(Load(0)) => {
                        rinsts.pop();
                        out.push(Dup);
                        out.push(decl);
                    }
                    _ => out.push(decl),
                }
            }
            &Store(ri) => {
                let store = rinsts.pop().unwrap();
                match rinsts.last() {
                    // Store(n),Load(n) => Dup,Store(n)
                    // avoids count byte, load op
                    Some(&Load(n)) if n == ri => {
                        rinsts.pop();
                        out.push(Dup);
                        out.push(store);
                    }
                    _ => out.push(store),
                }
            }
            JumpZero(_) | Jump(_) => {
                let jump = rinsts.pop().unwrap();
                let n = match jump {
                    JumpZero(n) | Jump(n) => n,
                    _ => unreachable!(),
                };
                match rinsts.last() {
                    Some(LabelMark(of)) if of == &n => {
                        // note we can't delete the LabelMark because there
                        // may be other jumps to it
                        // but it doesn't matter! because it's just a mark
                        // and doesn't become an instruction
                        out.push(rinsts.pop().unwrap());
                    }
                    _ => out.push(jump),
                }
            }
            DeVars(n) if n == &0 => {
                // just can it
                rinsts.pop();
            }
            _ => out.push(rinsts.pop().unwrap()),
        }
    }
    out
}

#[cfg(test)]
mod test {
    use super::*;
    #[test]
    fn multi_not() {
        let opt = peephole(&mut vec![Not, Not, Not, Push(5), Not, Not, Push(6)]);
        assert_eq!(opt, vec![Not, Push(5), Push(6)]);
    }
    #[test]
    fn decl_lit() {
        let opt = peephole(&mut vec![Push(5), Decl]);
        assert_eq!(opt, vec![DeclLit(5)]);
    }
    #[test]
    fn store_lit() {
        let opt = peephole(&mut vec![Push(5), Store(3)]);
        assert_eq!(opt, vec![StoreLit(3, 5)]);
    }
    #[test]
    fn xor_zero() {
        let opt = peephole(&mut vec![Push(0), Xor]);
        assert_eq!(opt, vec![]);
    }
    #[test]
    fn jump_zero() {
        let opt = peephole(&mut vec![Push(0), JumpZero(5)]);
        assert_eq!(opt, vec![Jump(5)]);
    }
    #[test]
    fn devars_zero() {
        let opt = peephole(&mut vec![DeVars(0)]);
        assert_eq!(opt, vec![]);
    }
    #[test]
    fn decl_load() {
        let opt = peephole(&mut vec![Push(5), Decl, Load(0)]);
        assert_eq!(opt, vec![Push(5), Dup, Decl]);
    }
    #[test]
    fn store_load() {
        let opt = peephole(&mut vec![Push(5), Store(4), Load(4)]);
        assert_eq!(opt, vec![Push(5), Dup, Store(4)]);
    }
    #[test]
    fn jump_same() {
        let opt = peephole(&mut vec![Jump(5), LabelMark(5)]);
        assert_eq!(opt, vec![LabelMark(5)]);
    }
    #[test]
    fn test_inlines() {
        let mut fns = vec![
            // inline to
            Fn {
                signature: Signature::default(),
                instructions: vec![
                    FnCall(1), // call the to be inlined
                    FnCall(2), // recursive, shouldn't be inlined
                ],
            },
            // to be inlined
            Fn {
                signature: Signature::default(),
                instructions: vec![Push(42), Return, Pop],
            },
            // recurses
            Fn {
                signature: Signature::default(),
                instructions: vec![FnCall(2)],
            },
        ];
        let expected = Fn {
            signature: Signature::default(),
            instructions: vec![
                Push(42),
                // handle the return
                Jump(0),
                Pop,
                LabelMark(0),
                FnCall(2),
            ],
        };
        inline_all(&mut fns, &mut 0);
        assert_eq!(fns[0], expected);
    }
    #[test]
    fn adj_ops() {
        let mut adj = vec![
            vec![0, 0, 0, 0],
            vec![0, 1, 0, 0],
            vec![2, 0, 0, 3],
            vec![0, 0, 4, 0],
        ];
        assert_eq!(col_sum(&mut adj, 0), 2);
        assert_eq!(col_sum(&mut adj, 1), 1);
        assert_eq!(col_sum(&mut adj, 2), 4);
        assert_eq!(col_sum(&mut adj, 3), 3);
        zero_col(&mut adj, 3);
        assert_eq!(col_sum(&mut adj, 3), 0);
    }
}
