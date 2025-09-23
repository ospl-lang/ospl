use std::rc::Rc;
use std::cell::RefCell;

use ospl::ospl::*;

fn make_test_ast() -> Block {
    Block(
        vec![
            Statement::Declaration {
                left: Expr::litstr("t"),
                right: Box::new(
                    Expr::Literal(
                        Value::Tuple(
                            vec![
                                Rc::new(RefCell::new(Value::SignedQuadrupleWord(1))),
                                Rc::new(RefCell::new(Value::SignedQuadrupleWord(2))),
                                Rc::new(RefCell::new(Value::SignedQuadrupleWord(3))),
                            ]
                        )
                    )
                )
            },
            Statement::Assign {
                left: Box::new(
                    Expr::Property(
                        Expr::var("t"),
                        Expr::litstr("0")
                    )
                ),
                right: Expr::litstr("hi")
            },
            Statement::Print {
                thing: Box::new(
                    Expr::Property(
                        Expr::var("t"),
                        Expr::litstr("0")
                    )
                )
            },
            Statement::Return(
                Expr::Property(
                    Expr::var("t"),
                    Expr::litstr("0")
                )
            )
        ]
    )
}

#[test]
fn tuple_prop_set() {
    let ast = make_test_ast();

    let root: interpreter::Context = interpreter::Context::new(None);
    let ctx: Rc<RefCell<interpreter::Context>> = Rc::new(RefCell::new(root));

    let result = interpreter::Interpreter::block(
        ctx,
        ast
    ).unwrap();

    assert!(*result.borrow() == Value::String("hi".into()))
}