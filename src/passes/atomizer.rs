use std::sync::Arc;

use crate::ast::{Expr, Program, Stmt};

///! Atomizer
///
/// removes complex expressions in statments
/// and transforms them in atomic statements
/// with single expressions
///
/// e.g.
/// let x = (1 + (2 + (3 + 4)))
/// =>
/// let y = (2 + (3 + 4)); let x = 1 + y
/// =>
/// let z = 3 + 4; let y = 2 + z; let x = 1 + y
pub fn atomize_stmts(program: Program) -> crate::Result<Program> {
    Ok(program)
}

// Creates let statements out of expressions
fn remove_complex_exprs(expr: Expr, let_stmts: &mut Vec<Stmt>) -> Expr {
    if is_atomized_expr(&expr) {
        return expr;
    }

    match expr {
        expr if is_atomized_expr(&expr) => expr,
        Expr::Neg(expr) => {
            let var = atomic_var(expr, let_stmts);
            Expr::Neg(var.arced())
        }
        Expr::Add(expr1, expr2) if is_atomized_expr(&expr1) => {
            let var = atomic_var(expr2, let_stmts);
            Expr::Add(expr1, var.arced())
        }
        Expr::Add(expr1, expr2) if is_atomized_expr(&expr2) => {
            let var = atomic_var(expr1, let_stmts);
            Expr::Add(var.arced(), expr2)
        }
        Expr::Add(expr1, expr2) => {
            let var1 = atomic_var(expr1, let_stmts);
            let var2 = atomic_var(expr2, let_stmts);
            Expr::Add(var1.arced(), var2.arced())
        }
        Expr::Var(_) => todo!(),
        Expr::Const(_) => todo!(),
        Expr::Sub(_, _) => todo!(),
    }
}

/// Returns an atomic var where all the  with all the
fn atomic_var(expr: Arc<Expr>, let_stmts: &mut Vec<Stmt>) -> Expr {
    let var_name = "tmp_0".to_string();
    let last_expr = remove_complex_exprs((*expr).clone(), let_stmts);

    let_stmts.push(Stmt::Let {
        binding: var_name.clone(),
        expr: last_expr,
    });

    Expr::Var("tmp_0".to_string())
}

// If it has been atomized it's means that it can't be atomized
// further
fn is_atomized_expr(expr: &Expr) -> bool {
    match expr {
        Expr::Const(_) => true,
        Expr::Var(_) => true,
        Expr::Neg(expr) => is_const_or_var(expr),
        Expr::Add(expr1, expr2) | Expr::Sub(expr1, expr2) => {
            is_const_or_var(expr1) && is_const_or_var(expr2)
        }
    }
}

fn is_const_or_var(expr: &Expr) -> bool {
    match expr {
        Expr::Const(_) => true,
        Expr::Var(_) => true,
        _ => false,
    }
}
