#![cfg(test)]

use std::{collections::HashSet, sync::Arc};

use crate::{
    ast::{Context, Expr, Program, Stmt, UnaryOp},
    passes,
};

#[test]
fn does_nothing_when_its_not_needed() {
    let context = Context::default();
    let input_stmts = vec![Stmt::Let {
        binding: "x".to_string(),
        expr: Expr::Const(3),
    }];

    let in_program = Program {
        context,
        stmts: input_stmts,
    };

    let out_program = passes::atomizer::atomize_stmts(in_program.clone()).unwrap();

    assert_eq!(in_program, out_program);
}

#[test]
fn unary_ops() {
    let context = Context::default();

    let input_stmts = vec![Stmt::Let {
        binding: "x".to_string(),
        expr: Expr::UnaryOp(
            UnaryOp::Neg,
            Arc::new(Expr::UnaryOp(UnaryOp::Neg, Arc::new(Expr::Const(3)))),
        ),
    }];

    let in_program = Program {
        context,
        stmts: input_stmts,
    };

    let out_program = passes::atomizer::atomize_stmts(in_program.clone()).unwrap();

    let expected_stmts = vec![
        Stmt::Let {
            binding: "tmp_0".to_string(),
            expr: Expr::UnaryOp(UnaryOp::Neg, Arc::new(Expr::Const(3))),
        },
        Stmt::Let {
            binding: "x".to_string(),
            expr: Expr::UnaryOp(UnaryOp::Neg, Arc::new(Expr::Var("tmp_0".to_string()))),
        },
    ];

    let expected_program = Program {
        context: Context {
            locals: HashSet::from(["tmp_0".to_string(), "x".to_string()]),
            stack_offset: 0,
        },
        stmts: expected_stmts,
    };

    assert_eq!(out_program, expected_program);
}

//   testCase "remove complex ops on Unary" $
//     let (Info locals 0, prog) = runComplexStmts [Let "x" (UnaryOp Neg (UnaryOp Neg (Const 8)))]
//         [var', "x"] = Set.toAscList locals
//      in case prog of
//           Program
//             [ Let var (UnaryOp Neg (Const 8)),
//               Let "x" (UnaryOp Neg (Var var1))
//               ] -> assertBool "tmp vars are not equal" (var == var1 && var' == var)
//           res -> assertBool ("It didn't construct the correct tmp vars, got: " <> show res) False,
//   -- --
//   --
//   testCase "remove complex ops do nothing with more complex ops but still atomic" $
//     let (Info locals 0, prog) = runComplexStmts [Let "x" (UnaryOp Neg (Const 8))]
//         expected = newProgram [Let "x" (UnaryOp Neg (Const 8))]
//      in do
//           assertEqual "" locals (Set.fromList ["x"])
//           assertEqual "" prog expected,
//   -- --
//   testCase "multiple stmts remove complex ops with Unary" $
//     let stmts =
//           [ Let "x" (UnaryOp Neg (UnaryOp Neg (Const 8))),
//             Let "y" (UnaryOp Neg (UnaryOp Neg (Const 8)))
//           ]
//         (Info locals 0, prog) = runComplexStmts stmts
//      in case prog of
//           Program
//             [ Let "tmp_0" (UnaryOp Neg (Const 8)),
//               Let "x" (UnaryOp Neg (Var "tmp_0")),
//               Let "tmp_1" (UnaryOp Neg (Const 8)),
//               Let "y" (UnaryOp Neg (Var "tmp_1"))
//               ] -> do
//               assertEqual "info is incorrect" locals (Set.fromList ["tmp_0", "x", "tmp_1", "y"])
//               assertBool "tmp vars are not equal" True
//           res -> assertBool ("It didn't construct the correct tmp vars, got: " <> show res) False,
//   -- -- --
//   testCase "remove nested Unary" $
//     let stmts = [Let "x" (UnaryOp Neg (UnaryOp Neg (UnaryOp Neg (Const 8))))]
//         atomStmts = progStmts . snd . runComplexStmts $ stmts
//      in case atomStmts of
//           [ Let "tmp_1" (UnaryOp Neg (Const 8)),
//             Let "tmp_0" (UnaryOp Neg (Var "tmp_1")),
//             Let "x" (UnaryOp Neg (Var "tmp_0"))
//             ] -> do
//               assertBool "tmp vars are not equal" True
//           res -> assertBool ("It didn't construct the correct tmp vars, got: " <> show res) False,
//   -- -- --
//   testCase "remove complex ops on Binary ops" $
//     let stmts = [Let "x" (BinOp Add (Const 10) (UnaryOp Neg (Const 8)))]
//         (Info locals 0, atomStmts) = runComplexStmts stmts
//      in case progStmts atomStmts of
//           [ Let "tmp_0" (UnaryOp Neg (Const 8)),
//             Let "x" (BinOp Add (Const 10) (Var "tmp_0"))
//             ] -> do
//               assertEqual "" locals (Set.fromList ["tmp_0", "x"])
//               assertBool "tmp vars are not equal" True
//           res ->
//             assertBool ("It didn't construct the correct tmp vars, got: " <> show res) False,
//   -- ------
//   testCase "multiple similar bindings discard first one" $
//     let stmts =
//           [ Let "x" (BinOp Add (Const 10) (Const 8)),
//             Let "x" (BinOp Add (Const 11) (Const 9))
//           ]
//         (Info locals 0, atomStmts) = runComplexStmts stmts
//      in case progStmts atomStmts of
//           [ Let "x" (BinOp Add (Const 10) (Const 8)),
//             Let "x" (BinOp Add (Const 11) (Const 9))
//             ] -> do
//               assertEqual "" ["x"] (Set.toAscList locals)
//               assertBool "tmp vars are not equal" True
//           res ->
//             assertBool ("It didn't construct the correct tmp vars, got: " <> show res) False,
//   ------
//   testCase "remove expr on Print" $
//     let stmts = [Print (BinOp Add (Const 10) (Const 8))]
//         (Info locals 0, atomStmts) = runComplexStmts stmts
//         [lvar] = Set.toAscList locals
//      in case progStmts atomStmts of
//           [Let lvar' (BinOp Add (Const 10) (Const 8)), Print (Var lvar'')] ->
//             assertBool "tmp vars are not equal" (lvar == lvar' && lvar' == lvar'')
//           res ->
//             assertBool ("It didn't construct the correct tmp vars, got: " <> show res) False,
//   -- -- --
//   testCase "remove more complex ops on Binary ops" $
//     let stmts = [Let "x" (BinOp Add (UnaryOp Neg (Const 8)) (Const 10))]
//         (Info locals 0, atomStmts) = runComplexStmts stmts
//         [lvar, "x"] = Set.toAscList locals
//      in case progStmts atomStmts of
//           [Let var (UnaryOp Neg (Const 8)), Let "x" (BinOp Add (Var var1) (Const 10))] ->
//             assertBool "tmp vars are not equal" (var == var1 && lvar == var)
//           res ->
//             assertBool ("It didn't construct the correct tmp vars, got: " <> show res) False,
//   -- -- --
//   testCase "remove even more complex ops on Binary ops" $
//     let stmts = [Let "x" (BinOp Add (UnaryOp Neg (Const 8)) (UnaryOp Neg (Const 10)))]
//         (Info locals 0, atomStmts) = runComplexStmts stmts
//         [lvar, lvar1, "x"] = Set.toAscList locals
//      in case progStmts atomStmts of
//           res@[Let var (UnaryOp Neg (Const 8)), Let var1 (UnaryOp Neg (Const 10)), Let "x" (BinOp Add (Var var') (Var var1'))] -> do
//             assertBool ("tmp vars are not equal, got" <> show res) (var == var' && var1 == var1' && var == lvar && var1 == lvar1)
//             assertBool ("tmp vars should be different, got: " <> show res) (var /= var1)
//           res -> assertBool ("It didn't construct the correct tmp vars, got: " <> show res) False
