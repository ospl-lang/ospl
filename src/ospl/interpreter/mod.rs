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
    pub fn expr(ctx: Rc<RefCell<Context>>, expr: Expr) -> Value {
        return match expr {
            // if it's a literal we can just unwrap the inner value
            Expr::Literal(v) => v,

            // it it's a variable, we try to get this variable
            Expr::Variable(name) => ctx.borrow().get(&name).expect("variable not found"),

            // if it's a function call, we go and handle that
            Expr::FunctionCall { name, args } => {
                let new_args: Vec<Value> =
                    args.iter().map(|arg| Interpreter::expr(ctx.clone(), arg.clone())).collect();

                // test
                match name.as_str() {
                    "print" => {
                        print!("{:?}", new_args);
                        return Value::Void
                    },
                    "println" => {
                        println!("{:?}", new_args);
                        return Value::Void
                    }
                    _ => return Self::do_function_call(ctx, name, new_args).unwrap_or(Value::Null)
                }
            }

            // if it's an operation, we do some dispath
            Expr::BinaryOp { left, right, op } => {
                // sorry to bring back trauma from C++
                // I just needed a variable name...
                let lvalue: Value = Self::expr(ctx.clone(), *left);
                let rvalue: Value = Self::expr(ctx.clone(), *right);

                // dispatch the correct op
                return match op.as_str() {
                    "+" => lvalue + rvalue,  // these return the same type as the lhs.
                    "-" => lvalue - rvalue,
                    "*" => lvalue * rvalue,
                    "/" => lvalue / rvalue,
                    "==" => Value::Bool(lvalue == rvalue),  // these are stupid and don't do that
                    "!=" => Value::Bool(lvalue != rvalue),
                    _ => panic!(">//< I don't know how to preform '{}'!", op)
                }
            },

            // it it's a loop, we can handle that
            Expr::Loop(body) => match Self::do_loop(ctx, *body) {
                StatementControl::Break(v) => v,
                StatementControl::EarlyReturn(v) => v,
                _ => panic!(">//< loops should return something")
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
            Statement::VariableAssignment { left, right } => {
                let val = Self::expr(Rc::clone(&ctx), *right);
                ctx.borrow_mut().set(&left, val);
                return StatementControl::Default
            },
            Statement::Expression(x) => {
                Self::expr(Rc::clone(&ctx), x);
                return StatementControl::Default
            },
            Statement::Return(x) => {
                return StatementControl::EarlyReturn(Self::expr(ctx, x))
            },
            Statement::Break(x) => {
                return StatementControl::Break(Self::expr(ctx, x))
            },
            Statement::Continue => {
                return StatementControl::Continue
            },
            Statement::If { condition, on_true } => {
                let condition_return: bool = Self::expr(ctx.clone(), condition)
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
            },
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
    pub fn block(ctx: Rc<RefCell<Context>>, body: Block) -> Option<Value> {
        for stmt in body.0 {
            let control: StatementControl = Self::stmt(Rc::clone(&ctx), stmt);
            match control {
                StatementControl::EarlyReturn(x) => return Some(x),
                StatementControl::Break(_) => panic!("tried to break outside a loop!"),
                StatementControl::Continue => panic!("tried to continue outside a loop!"),
                StatementControl::Default => continue
            }
        };

        return None
    }
}
