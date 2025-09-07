#[derive(Debug)]
pub enum StatementControl {
    Default,
    EarlyReturn(Value),
    Break(Value),
    Continue,
}

use super::*;
use std::rc::Rc;
use std::cell::RefCell;

pub mod context;
pub use context::*;

/// executes and evaluates OSPL ASTs
pub struct Interpreter;
pub mod function;
pub mod loops;
impl Interpreter {
    fn solve_for_avbk(
        ctx: &Rc<RefCell<Context>>,
        a: Box<Expr>,
        b: Box<Expr>,
    ) -> (Rc<RefCell<Value>>, String) {
        // Resolve a_key
        let a_key = Self::expr(ctx.clone(), *a)
            .borrow()

            // safe to clone, we literally could not care less about
            // mutability here
            .clone()
            .into_id();

        // Grab the actual container from the environment
        let a_value_ref = ctx
            .borrow()
            .get(&a_key)
            .expect(">//< expected ID")

            // safe to clone, we clone the Rc, incrementing the refcount.
            // this is to help you, Oscar. You're welcome ^_^
            .clone();

        // Resolve b_key
        let b_key = Self::expr(ctx.clone(), *b)
            .borrow()

            // safe to clone, we literally could not care less about
            // mutability here, we only care abotu the ID
            .clone()
            .into_id();

        (a_value_ref, b_key)
    }

    /// Evaluates an `Expr`
    /// 
    /// # Arguments
    /// 
    /// * `ctx` - The context to evaluate this expression in.
    ///           This context is mutated
    /// * `expr` - The `Expr` to evaluate.
    /// 
    /// # Returns
    /// 
    /// The value of this Expr, as `Value`
    pub fn expr(ctx: Rc<RefCell<Context>>, expr: Expr) -> Rc<RefCell<Value>> {
        return match expr {
            // if it's a literal we can just unwrap the inner value
            Expr::Literal(v) => Rc::new(RefCell::new(v)),

            // it it's a variable, we try to get this variable
            // cloning here doesn't ACTUALLY clone the value, it just
            // increments its reference count.
            Expr::Variable(left) => {
                let id = Self::expr(ctx.clone(), *left).borrow().clone().into_id();

                ctx.borrow()
                    .get(&id)
                    .expect("expected ID")
                    .clone() // cheap Rc clone
            }

            // it it's a variable, we try to get this variable
            // I completely chatgpt'd this one too
            // I have no idea what's going on
            // and Kevin I know that's bad practice Rust is hard as fuck okay...
            // I don't want to waste 6 days refactoring half the fuckin codebase
            // because I need property access.
            Expr::Property(a, b) => {
                let (a_value, b_key) = Self::solve_for_avbk(&ctx, a, b); // Rc<RefCell<Value>>

                let val: Rc<RefCell<Value>> = match &*a_value.borrow() {
                    Value::Tuple(t) => {
                        let idx: usize = b_key.parse::<usize>().expect(">//< non-integer tuple index");

                        t
                            .get(idx)
                            .expect(">//< bad tuple index")
                            .clone()
                    },
                    Value::Mixmap { ordered, keyed } => {
                        if let Ok(idx) = b_key.parse::<usize>() {
                            // Rc clone
                            ordered
                                .get(idx)
                                .expect(">//< bad mixmap index")
                                .clone()
                        } else {
                            // Rc clone
                            keyed
                                .get(&b_key)
                                .expect(">//< bad key")
                                .clone()
                        }
                    }
                    _ => a_value.clone()  // Already Rc<RefCell<Value>>
                };

                return val
            }

            // if it's a function call, we go and handle that
            Expr::FunctionCall { left, args } => {
                let name = Self::expr(ctx.clone(), *left).borrow().clone().into_id();

                let new_args: Vec<Rc<RefCell<Value>>> = args
                    .iter()
                    .map(|arg| Interpreter::expr(ctx.clone(), arg.clone()).clone())
                    .collect();

                    return Self::do_function_call(ctx, name, new_args)
                        .unwrap_or_else(|| Rc::new(RefCell::new(Value::Null)))
            }

            // if it's an operation, we do some dispath
            Expr::BinaryOp { left, right, op } => {
                // sorry to bring back trauma from C++
                // I just needed a variable name...

                // make a copy here because we don't want to mess with the original
                let lvalue: Value = Self::expr(ctx.clone(), *left).borrow().clone();
                let rvalue: Value = Self::expr(ctx.clone(), *right).borrow().clone();

                // dispatch the correct op
                return Rc::new(RefCell::new(match op.as_str() {
                    "+" => lvalue + rvalue,  // these return the same type as the lhs.
                    "-" => lvalue - rvalue,
                    "*" => lvalue * rvalue,
                    "/" => lvalue / rvalue,
                    "==" => Value::Bool(lvalue == rvalue),  // these are stupid and don't do that
                    "!=" => Value::Bool(lvalue != rvalue),
                    _ => unreachable!(">//< I don't know how to preform '{}'!", op)
                }))
            },

            // it it's a loop, we can handle that
            Expr::Loop(body) => match Self::do_loop(ctx, *body) {
                StatementControl::Break(v) => Rc::new(RefCell::new(v)),
                StatementControl::EarlyReturn(v) => Rc::new(RefCell::new(v)),
                _ => unreachable!(">//< I WAS THREE SHEDLETSKIES AWAY HOW DID BRO HIT ME??!")
            },
        }
    }
    
    /// Executes a `Statement`
    /// 
    /// # Arguments
    /// 
    /// * `ctx` - The context in which to execute the statement.
    ///           This context is mutated.
    /// * `stmt` - The `Statement` to execute.
    /// 
    /// # Returns
    /// 
    /// The control flow of the statement as `StatementControl`
    pub fn stmt(ctx: Rc<RefCell<Context>>, stmt: Statement) -> StatementControl {
        match stmt {
            Statement::Assign { left, right } => {
                let var = Self::expr(ctx.clone(), *left);
                let lit = Self::expr(ctx.clone(), *right)
                    .borrow()
                    .clone();

                *var.borrow_mut() = lit;
                return StatementControl::Default
            },

            Statement::Expression(x) => {
                Self::expr(ctx.clone(), x);
                return StatementControl::Default
            },

            Statement::Return(x) => {
                return StatementControl::EarlyReturn(
                    Self::expr(ctx, x)
                    .borrow()
                    .clone()
                )
            },
            Statement::Break(x) => {
                return StatementControl::Break(
                    Self::expr(ctx, x)
                    .borrow()
                    .clone()
                )
            },

            Statement::Continue => {
                return StatementControl::Continue
            },

            Statement::If { condition, on_true } => {
                let condition_return: bool = Self::expr(ctx.clone(), condition).borrow()
                    .truthiness();

                if condition_return {
                    let newctx: Rc<RefCell<Context>> =
                        Rc::new(RefCell::new(Context::new(Some(ctx.clone()))));

                    for stmt in on_true.0 {
                        match Self::stmt(newctx.clone(), stmt) {
                            StatementControl::Default => {}         // do nothing
                            ctrl => return ctrl,  // return everything else
                        }
                    }
                };
                return StatementControl::Default
            }
        }
    }

    /// Executes a `Block`
    /// 
    /// # Arguments
    /// 
    /// * `ctx` - The context in which to execute the block.
    ///           This context is mutated.
    /// * `body` - The `Block` to execute
    /// 
    /// # Returns
    /// 
    /// The return of this block, if there is one, as `Option<Value>`
    pub fn block(ctx: Rc<RefCell<Context>>, body: Block) -> Option<Rc<RefCell<Value>>> {
        for stmt in body.0 {
            let control: StatementControl = Self::stmt(ctx.clone(), stmt);
            match control {
                StatementControl::EarlyReturn(x) => return Some(Rc::new(RefCell::new(x))),
                StatementControl::Break(_) => panic!("tried to break outside a loop!"),
                StatementControl::Continue => panic!("tried to continue outside a loop!"),
                StatementControl::Default => continue
            }
        };

        return None
    }
}
