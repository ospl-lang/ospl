pub mod ospl;
use std::cell::RefCell;
use std::rc::Rc;

use crate::ospl::*;

fn main() {
    let root: interpreter::Context = interpreter::Context::new(None);
    let ctx: Rc<RefCell<interpreter::Context>> = Rc::new(RefCell::new(root));

    let stmt: ospl::Statement = ospl::Statement::VarDeclaration {
        left: Expr::litstr("add"),
        right: Box::new(
            ospl::Expr::Literal(
                ospl::Value::Function {
                    spec: vec![
                        Subspec::Bind("a".into()),
                        Subspec::Bind("b".into()),
                    ],
                    body: ospl::Block(vec![
                        // infinite loop
                        Statement::VarDeclaration {
                            left: Expr::litstr("i"),
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
                                        Statement::Print {
                                            thing: Expr::var("i")
                                        },
                                        Statement::Assign {
                                            left: Expr::var("i"),
                                            right: Box::new(Expr::BinaryOp {
                                                left: Expr::var("i"),
                                                right: Expr::s_qword(1),
                                                op: "+".into()
                                            })
                                        },
                                        Statement::If {
                                            condition: Expr::BinaryOp {
                                                left: Expr::var("i"),
                                                right: Expr::s_qword(10),
                                                op: "==".into()
                                            },
                                            on_true: Block(vec![
                                                Statement::Return(*Expr::var("i"))
                                            ]),
                                            on_false: None
                                        },
                                    ])
                                )
                            )
                        ),

                        // return test
                        Statement::Return(Expr::BinaryOp {
                            left: Expr::var("a"),
                            right: Expr::var("b"),
                            op: "+".into()
                        })
                    ])
                }
            )
        )
    };

    println!("yay");
    // interpreter::Interpreter::stmt(ctx.clone(), stmt.clone());
    /* interpreter::Interpreter::block(
        ctx,
        Block(vec![
            stmt.clone(),
            Statement::Expression(
                Expr::FunctionCall {
                    left: Expr::var("add"),
                    args: vec![
                        Expr::Literal(Value::QuadrupleWord(9)),
                        Expr::Literal(Value::QuadrupleWord(10)),
                    ]
                }
            ),
        ])
    ); */

    parser::main();
}
