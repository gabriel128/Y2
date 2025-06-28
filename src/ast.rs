#![allow(dead_code)]

use std::{collections::HashSet, sync::Arc};

#[derive(PartialEq, Eq, Debug, Clone)]
pub enum Type {
    ToInfer,
    Native,
}

#[derive(PartialEq, Eq, Debug, Clone)]
pub enum UnaryOp {
    Neg,
}

#[derive(PartialEq, Eq, Debug, Clone)]
pub enum BinOp {
    Add,
    Sub,
    Mul,
    Div,
}

#[derive(PartialEq, Eq, Debug, Clone)]
pub enum NativeVal {
    I64(i64),
    U64(u64),
}

#[derive(PartialEq, Eq, Debug, Clone)]
pub enum Expr {
    Const(NativeVal),
    UnaryOp(Type, UnaryOp, Arc<Expr>),
    BinOp(Type, BinOp, Arc<Expr>, Arc<Expr>),
    Var(Type, String),
}

impl Expr {
    pub fn arced(self) -> Arc<Self> {
        Arc::new(self)
    }

    pub fn ty(&self) -> &Type {
        match self {
            Expr::Const(..) => &Type::Native,
            Expr::UnaryOp(ty, ..) => ty,
            Expr::BinOp(ty, ..) => ty,
            Expr::Var(ty, ..) => ty,
        }
    }

    pub fn is_const_or_var(&self) -> bool {
        match self {
            Expr::Const(..) => true,
            Expr::Var(..) => true,
            _ => false,
        }
    }
}

#[derive(PartialEq, Eq, Clone, Debug)]
pub enum Stmt {
    Let {
        ty: Type,
        binding: String,
        expr: Expr,
    },
    DebugPrint(Expr),
    Return(Type, Expr),
}

#[derive(Debug, Clone, PartialEq, Eq, Default)]
pub struct Context {
    /// Locals Vars are meant to be unique.
    /// But there is no place in the stack ensured
    pub locals: HashSet<String>,
    /// Current stack offset
    pub stack_offset: i32,
}

/// A program is a sequence of statements
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Program {
    pub context: Context,
    pub stmts: Vec<Stmt>,
}
