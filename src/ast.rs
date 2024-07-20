#![allow(dead_code)]

use std::{collections::HashSet, sync::Arc};

#[derive(Debug, Clone)]
pub enum Expr {
    Const(i32),
    Neg(Arc<Expr>),
    Add(Arc<Expr>, Arc<Expr>),
    Sub(Arc<Expr>, Arc<Expr>),
    Var(String),
}

impl Expr {
    pub fn arced(self) -> Arc<Self> {
        Arc::new(self)
    }
}

#[derive(Debug)]
pub enum Stmt {
    Let { binding: String, expr: Expr },
    DebugPrint(Expr),
    Return(Expr),
}

#[derive(Debug)]
pub struct Context {
    /// Locals Vars are meant to be unique.
    /// But there is no place in the stack ensured
    pub locals: HashSet<String>,
    /// Current stack offset
    pub stack_offset: i32,
}

/// A program is a sequence of statements
#[derive(Debug)]
pub struct Program {
    pub context: Context,
    pub stmts: Vec<Stmt>,
}

#[cfg(test)]
mod tests {
    use super::{Expr, Stmt};

    #[test]
    fn builds_simple_add_expr() {
        Expr::Add(Expr::Const(3).arced(), Expr::Const(4).arced());
    }

    #[test]
    fn builds_simple_stmt() {
        let expr = Expr::Add(Expr::Const(3).arced(), Expr::Const(4).arced());

        Stmt::Let {
            binding: "blah".to_string(),
            expr,
        };
    }
}
