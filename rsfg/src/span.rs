/// a span is simply a location in the program (in a file, but there's no
/// multi-file support yet), from the beginning (lo) to end (hi) represented
/// as (line, col). You'll find them in tokens and a sprinkled all around AST nodes
///
/// Note that i mentioned no multi-file support despite there being a
/// standard library? well you'll be glad to know that actually the stdlib is
/// simply appended to the source file, so sometimes errors will refer to lines
/// higher than the source length when there's a problem in the stdlib,
/// or even just one triggered by code errors that refer to the stdlib
#[derive(PartialEq, Eq, Clone, Copy, Debug, Default)]
pub struct Span {
    pub lo: (usize, usize),
    pub hi: (usize, usize),
    // TODO: file
}
impl Span {
    pub fn new() -> Self {
        Self::default()
    }
    /// This is set as in "a set of berries" not "set x to 2". taking a list
    /// of spans, find the span spanning ALL of them, so for example one
    /// being just on line 1 and one on line 3, the set will go from line
    /// 1 to 3. This is used to make up for the incredibly weird smattering
    /// of spans around the AST
    pub fn set(mut spans: Vec<Span>) -> Span {
        let first = spans.pop().expect("cannot form set of less than one span");
        let mut lo = first.lo;
        let mut hi = first.hi;
        for span in spans {
            // if lower, go lower
            if span.lo.0 < lo.0 || (span.lo.0 == lo.0 && span.lo.1 < lo.1) {
                lo = span.lo;
            }
            // if higher go higher
            if span.hi.0 > hi.0 || (span.hi.0 == hi.0 && span.hi.1 > hi.1) {
                hi = span.hi;
            }
        }
        Span { lo, hi }
    }
}
impl std::fmt::Display for Span {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        if *self == Span::new() {
            write!(f, "internal")
        } else {
            write!(f, "{}:{}", self.lo.0, self.lo.1)
        }
    }
}

#[cfg(test)]
mod test {
    use super::Span;
    #[test]
    fn test_span_set() {
        let set = Span::set(vec![
            Span {
                lo: (4, 4),
                hi: (5, 5),
            },
            Span {
                lo: (4, 2),
                hi: (4, 10),
            },
        ]);
        assert_eq!(
            set,
            Span {
                lo: (4, 2),
                hi: (5, 5)
            }
        );
    }
}

impl crate::ast::Expression {
    pub fn full_span(&self) -> Span {
        match self {
            Self::Literal(lit) => lit.span,
            Self::Identifier(id) => id.span,
            Self::Not(expr) => {
                warn!("unimplemented span on not to include ! symbol");
                expr.full_span()
            }
            // A FnCall can be an expression as well as a statement
            // A statement FnCall is lowered differently than an expression FnCall
            Self::FnCall(call) => call.span,
            Self::Binary(binary) => {
                Span::set(vec![binary.left.full_span(), binary.right.full_span()])
            }
        }
    }
}
