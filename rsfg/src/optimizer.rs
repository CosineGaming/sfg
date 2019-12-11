/// optimization pass over low level bytecode
use crate::llr::{*, Instruction::*};

/// general principles:
/// MORE INSTRUCTIONS IS WORSE almost all the time (reading from bytecode is slow)
/// within instructions, the stack is FAST (caching), much faster than Load/Store
/// method calls are worse yet

/// should never error, because given a valid program bytecode, and outputs
/// one too
pub fn optimize_llr(mut llr: LLR) -> LLR {
    // CHECK: should it be in optimize or lower??
    let mut inst_uses = vec![];
    inst_uses.resize(llr.fns.len(), 0);
    for func in &llr.fns {
        for inst in &func.instructions {
            if let FnCall(i) = inst {
                inst_uses[*i as usize] += 1;
            }
        }
    }
    // TODO: remove unused functions
    // problem being, fns are all public so "unused" is hard to describe (no main)
    // options: declare main as entry point, add public tag
    // or better just do it from the std library when we have imports
    //llr.fns = llr.fns.iter().enumerate().filter(|(func_n, func)| {
        //if inst_uses[*func_n] == 0 {
            //// TODO: better system for this!!! Add warnings and return them
            //eprintln!("[WARNING] unused function {}", func.signature.name);
            //false
        //} else { true }
    //}).map(|(n, f)| *f).collect();
    for func in &mut llr.fns {
        func.instructions = optimize_body(&mut func.instructions);
    }
    debug!("{}", llr);
    llr
}

fn optimize_body(insts: &mut Vec<Instruction>) -> Vec<Instruction> {
    insts.reverse();
    let rinsts = insts;
    let mut out = vec![];

    // peephole
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
    // peephole
    #[test]
    fn multi_not() {
        let opt = optimize_body(&mut vec![Not, Not, Not, Push(5), Not, Not, Push(6)]);
        assert_eq!(opt, vec![Not, Push(5), Push(6)]);
    }
    #[test]
    fn decl_lit() {
        let opt = optimize_body(&mut vec![Push(5), Decl]);
        assert_eq!(opt, vec![DeclLit(5)]);
    }
    #[test]
    fn store_lit() {
        let opt = optimize_body(&mut vec![Push(5), Store(3)]);
        assert_eq!(opt, vec![StoreLit(3, 5)]);
    }
    #[test]
    fn xor_zero() {
        let opt = optimize_body(&mut vec![Push(0), Xor]);
        assert_eq!(opt, vec![]);
    }
    #[test]
    fn jump_zero() {
        let opt = optimize_body(&mut vec![Push(0), JumpZero(5)]);
        assert_eq!(opt, vec![Jump(5)]);
    }
    #[test]
    fn devars_zero() {
        let opt = optimize_body(&mut vec![DeVars(0)]);
        assert_eq!(opt, vec![]);
    }
    #[test]
    fn decl_load() {
        let opt = optimize_body(&mut vec![Push(5), Decl, Load(0)]);
        assert_eq!(opt, vec![Push(5), Dup, Decl]);
    }
    #[test]
    fn store_load() {
        let opt = optimize_body(&mut vec![Push(5), Store(4), Load(4)]);
        assert_eq!(opt, vec![Push(5), Dup, Store(4)]);
    }
}
