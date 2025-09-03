pub mod ospl;
use std::cell::RefCell;
use std::rc::Rc;

use crate::ospl::*;

fn main() {
    let root: interpreter::Context = interpreter::Context::new(None);
    let ctx: Rc<RefCell<interpreter::Context>> = Rc::new(RefCell::new(root));

    let stmt: ospl::Statement = ospl::Statement::VariableAssignment {
        left: "add".into(),
        right: Box::new(
            ospl::Expr::Literal(
                ospl::Value::Function {
                    spec: vec![
                        Subspec::Bind("a".into()),
                        Subspec::Bind("b".into()),
                    ],
                    body: ospl::Block(vec![
                        // infinite loop
                        Statement::VariableAssignment {
                            left: "i".into(),
                            right: Box::new(
                                Expr::Literal(
                                    Value::QuadrupleWord(0)
                                )
                            )
                        },
                        Statement::Expression(
                            Expr::Loop(
                                Box::new(
                                    Block(vec![
                                        Statement::VariableAssignment {
                                            left: "i".into(),
                                            right: Box::new(Expr::BinaryOp {
                                                left: Box::new(Expr::Variable("i".into())),
                                                right: Box::new(Expr::Literal(Value::QuadrupleWord(1))),
                                                op: "+".into()
                                            })
                                        },
                                        Statement::Expression(Expr::FunctionCall {
                                            name: "println".into(),
                                            args: vec![
                                                Expr::Variable("i".into())
                                            ]
                                        }),
                                        Statement::If {
                                            condition: Expr::BinaryOp {
                                                left: Box::new(Expr::Variable("i".into())),
                                                right: Box::new(Expr::Literal(Value::QuadrupleWord(10))),
                                                op: "==".into()
                                            },
                                            on_true: Block(vec![
                                                Statement::Return(Expr::Literal(Value::QuadrupleWord(69727420)))
                                            ])
                                        },
                                    ])
                                )
                            )
                        ),

                        // return test
                        Statement::Return(Expr::BinaryOp {
                            left: Box::new(Expr::Variable("a".into())),
                            right: Box::new(Expr::Variable("b".into())),
                            op: "+".into()
                        })
                    ])
                }
            )
        )
    };
    // interpreter::Interpreter::stmt(ctx.clone(), stmt.clone());
    interpreter::Interpreter::block(
        ctx,
        Block(vec![
            stmt.clone(),
            Statement::Expression(
                Expr::FunctionCall {
                    name: "add".into(),
                    args: vec![
                        Expr::Literal(Value::QuadrupleWord(9)),
                        Expr::Literal(Value::QuadrupleWord(10)),
                    ]
                }
            ),
        ]));

    //println!("{:#?}", stmt.clone());
}
