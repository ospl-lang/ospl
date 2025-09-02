use super::*;
use std::collections::HashMap;
use std::rc::{Rc, Weak};
use std::cell::RefCell;

/// Context in which an AST is executed.
#[derive(Debug, Clone)]
pub struct Context {
    /// A reference to parent context.
    /// 
    /// This is needed so that you can climb up the chain to find variables
    /// and items above the current context.
    parent: Option<Weak<RefCell<Context>>>,

    /// A hashmap of symbols in this context. Self-explainatory
    vars: HashMap<String, Value>,
}

impl Context {
    pub fn new(parent: Option<Rc<RefCell<Context>>>) -> Self {
        Self {
            vars: HashMap::new(),
            parent: parent.as_ref().map(|p| Rc::downgrade(p)),
        }
    }

    pub fn get(&self, key: &str) -> Option<Value> {
        if let Some(v) = self.vars.get(key) {
            Some(v.clone())
        } else {
            self.parent.as_ref()?.upgrade()?.borrow().get(key)
        }
    }

    pub fn set(&mut self, key: &str, value: Value) {
        // Start with the current context
        let mut current_parent = self.parent.as_ref().and_then(|p| p.upgrade());

        // If the key exists locally, just update it
        if self.vars.contains_key(key) {
            self.vars.insert(key.to_string(), value);
            return;
        }

        // Walk up the parent chain iteratively
        while let Some(parent_rc) = current_parent {
            let mut parent_ctx = parent_rc.borrow_mut();
            if parent_ctx.vars.contains_key(key) {
                parent_ctx.vars.insert(key.to_string(), value);
                return;
            }
            current_parent = parent_ctx.parent.as_ref().and_then(|p| p.upgrade());
        }

        // Key wasn’t found anywhere, insert locally
        self.vars.insert(key.to_string(), value);
    }
}

/// executes and evaluates OSPL ASTs
pub struct Interpreter;
impl Interpreter {
    /// Unwraps a function spec
    /// 
    /// # Arguments
    /// 
    /// * `ctx` - The context to inject the values into.
    ///           This context is mutated
    /// * `spec` - The spec to unwrap
    /// * `args` - The args to unwrap with
    fn unwrap_fn_spec(ctx: Rc<RefCell<Context>>, spec: Vec<Subspec>, args: Vec<Value>) {
        debug_assert!(spec.len() == args.len());  // should ALWAYS be true

        for (subspec, arg) in spec.into_iter().zip(args.into_iter()) {
            match subspec {
                Subspec::Bind(key) => {
                    let _ = ctx.borrow_mut().set(&key, arg);
                },
                Subspec::Destruct(tree) => Self::unwrap_fn_spec(ctx.clone(), tree, arg.into_values()),
                _ => panic!(">//< I don't know what to do here")
            }
        }
        return;
    }

    /// Handles a loop
    /// 
    /// # Arguments
    /// 
    /// * `ctx` - The contxet in which to evaluate the loop
    /// * `body` - The body of the loop
    /// 
    /// # Returns
    /// 
    /// The return, if there is one, as `StatementControl` (with data attached
    /// if available)
    fn do_loop(ctx: Rc<RefCell<Context>>, body: Block) -> StatementControl {
        'outer: loop {
            for stmt in &body.0 {   // iterate by reference to avoid cloning
                let result = Self::stmt(ctx.clone(), stmt.clone());
                match result {
                    StatementControl::Break(_) => return result,
                    StatementControl::EarlyReturn(_) => return result,
                    StatementControl::Continue => continue 'outer, // continue loop properly
                    _ => {}  // do nothing, just move to next stmt
                }
            }
        }
    }

    /// Handles a function call
    /// 
    /// # Arguments
    /// 
    /// * `ctx` - The context in which to find and run the function
    /// * `name` - The name of the symbol assigned to the function
    /// * `args` - The arguments to run the function with.
    ///            Must be already evaluated into `Value`s
    /// 
    /// # Returns
    /// 
    /// The return value of the function, if there is one, as `Option<Value>`
    fn do_function_call(ctx: Rc<RefCell<Context>>, name: String, args: Vec<Value>) -> Option<Value> {
        // create a child context for the function
        let child_ctx = Rc::new(
            RefCell::new(
                Context::new(
                    Some(ctx.clone()))));

        // I fscking love Rust for this reason, let-else syntax is amazing!!
        let Value::Function { spec, body } = ctx.borrow().get(&name)
            .expect(&format!("expected function `{}` to exist", name))
        else {
            panic!("invalid function!")
        };
        Self::unwrap_fn_spec(child_ctx.clone(), spec, args);

        // loop through all statements and run them
        return Self::block(child_ctx, body)
    }

    /// Evaluates an `ast::Expr`
    /// 
    /// # Arguments
    /// 
    /// * `ctx` - The context to evaluate this expression in.
    ///           This context is mutated
    /// * `expr` - The `ast::Expr` to evaluate.
    /// 
    /// # Returns
    /// 
    /// The value of this Expr, as `ast::Value`
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
    
    /// Executes an `ast::Statement`
    /// 
    /// # Arguments
    /// 
    /// * `ctx` - The context in which to execute the statement.
    ///           This context is mutated.
    /// * `stmt` - The `ast::Statement` to execute.
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

    /// Executes an `ast::Block`
    /// 
    /// # Arguments
    /// 
    /// * `ctx` - The context in which to execute the block.
    ///           This context is mutated.
    /// * `body` - The `ast::Block` to execute
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

#[derive(Debug)]
pub enum StatementControl {
    Default,
    EarlyReturn(Value),
    Break(Value),
    Continue,
}