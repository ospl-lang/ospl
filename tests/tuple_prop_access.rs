use std::rc::Rc;
use std::cell::RefCell;

use ospl::ospl::*;

fn make_test_ast() -> Block {
    Block(
        vec![
            Statement::VarDeclaration {
                left: Expr::litstr("t"),
                right: Box::new(
                    Expr::Literal(
                        Value::Tuple(
                            vec![
                                Rc::new(RefCell::new(Value::SignedQuadruleWord(1))),
                                Rc::new(RefCell::new(Value::SignedQuadruleWord(2))),
                                Rc::new(RefCell::new(Value::SignedQuadruleWord(3))),
                            ]
                        )
                    )
                )
            },
            Statement::Print {
                thing: Box::new(
                    Expr::Property(
                        Expr::var("t"),
                        Expr::litstr("1"),
                    )
                )
            }
        ]
    )
}

#[test]
fn tuple_prop_access() {
    let ast = make_test_ast();

    let root: interpreter::Context = interpreter::Context::new(None);
    let ctx: Rc<RefCell<interpreter::Context>> = Rc::new(RefCell::new(root));

    let _ = interpreter::Interpreter::block(
        ctx,
        ast
    );
}