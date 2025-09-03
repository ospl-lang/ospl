use super::*;

use std::rc::Rc;
use std::cell::RefCell;

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
    pub fn do_function_call(ctx: Rc<RefCell<Context>>, name: String, args: Vec<Value>) -> Option<Value> {
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
}