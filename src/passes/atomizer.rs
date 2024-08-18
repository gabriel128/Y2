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
    let atomized_stmts = remove_complex_stmts(program.stmts)?;
    let atomized_program = Program {
        context: program.context,
        stmts: atomized_stmts,
    };
    Ok(atomized_program)
}

//
fn remove_complex_stmts(stmts: Vec<Stmt>) -> crate::Result<Vec<Stmt>> {
    let mut atomized_stmts = Vec::new();

    for stmt in stmts {
        let mut new_stmts = remove_complex_expr_from_stmt(stmt)?;
        atomized_stmts.append(&mut new_stmts);
    }

    Ok(atomized_stmts)
}

/// Transform a complex statment (i.e. statements that are not atomic)
/// into sequential let bindings ending with the originl stmt transformed
fn remove_complex_expr_from_stmt(stmt: Stmt) -> crate::Result<Vec<Stmt>> {
    match stmt {
        Stmt::Let { binding, expr } => {
            let mut stmts = Vec::new();
            let last_expr = remove_complex_exprs(expr, &mut stmts);
            // TODO add local vars
            stmts.push(Stmt::Let {
                binding,
                expr: last_expr,
            });
            Ok(stmts)
        }
        Stmt::DebugPrint(_) => todo!(),
        Stmt::Return(_) => todo!(),
    }
}
// Creates let statements out of an expression and return the
// last expression as a var
fn remove_complex_exprs(expr: Expr, atomized_stmts: &mut Vec<Stmt>) -> Expr {
    if is_atomized_expr(&expr) {
        return expr;
    }

    match expr {
        expr if is_atomized_expr(&expr) => expr,
        Expr::UnaryOp(op, expr) => {
            let var = atomic_var(expr, atomized_stmts);
            Expr::UnaryOp(op, var.arced())
        }
        Expr::BinOp(op, expr1, expr2) if is_atomized_expr(&expr1) => {
            let var = atomic_var(expr2, atomized_stmts);
            Expr::BinOp(op, expr1, var.arced())
        }
        Expr::BinOp(op, expr1, expr2) if is_atomized_expr(&expr2) => {
            let var = atomic_var(expr1, atomized_stmts);
            Expr::BinOp(op, var.arced(), expr2)
        }
        Expr::BinOp(op, expr1, expr2) => {
            let var1 = atomic_var(expr1, atomized_stmts);
            let var2 = atomic_var(expr2, atomized_stmts);
            Expr::BinOp(op, var1.arced(), var2.arced())
        }
        Expr::Var(_) => todo!(),
        Expr::Const(_) => todo!(),
    }
}

/// Returns an atomic var where all the  with all the
fn atomic_var(expr: Arc<Expr>, atomized_stmts: &mut Vec<Stmt>) -> Expr {
    // TODO
    // - remove hardcoded var_name
    // - add local vars
    let var_name = "tmp_0".to_string();
    let last_expr = remove_complex_exprs((*expr).clone(), atomized_stmts);

    atomized_stmts.push(Stmt::Let {
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
        Expr::UnaryOp(_, expr) => is_const_or_var(expr),
        Expr::BinOp(_, expr1, expr2) => is_const_or_var(expr1) && is_const_or_var(expr2),
    }
}

fn is_const_or_var(expr: &Expr) -> bool {
    match expr {
        Expr::Const(_) => true,
        Expr::Var(_) => true,
        _ => false,
    }
}
