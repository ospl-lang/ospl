use super::*;

use std::rc::Rc;
use std::cell::RefCell;

impl Interpreter {
    /// Destruct into a context
    /// 
    /// # Arguments
    /// 
    /// * `ctx` - The context to inject the values into.
    ///           This context is mutated
    /// * `spec` - The spec to unwrap
    /// * `args` - The args to unwrap with
    pub fn destruct_into(ctx: Rc<RefCell<Context>>, spec: Vec<Subspec>, args: Vec<Rc<RefCell<Value>>>) {
        debug_assert!(spec.len() == args.len());  // should ALWAYS be true

        for (subspec, arg) in spec.into_iter().zip(args.into_iter()) {
            match subspec {
                Subspec::Bind(key) => {
                    ctx.borrow_mut().set(&key, arg.borrow().clone());
                },
                Subspec::Destruct(tree) => Self::destruct_into(
                    ctx.clone(),
                    tree,
                    arg.borrow().clone().into_values()
                ),
                Subspec::Ignore => {},
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
    pub fn do_function_call(
        ctx: Rc<RefCell<Context>>,
        name: String,
        args: Vec<Rc<RefCell<Value>>>
    ) -> Option<Rc<RefCell<Value>>> {

        // create child context
        let child_ctx = Rc::new(RefCell::new(Context::new(Some(ctx.clone()))));

        // get the function Value from the context
        let func_rc = ctx.borrow().get(&name).expect("expected ID"); // Rc<RefCell<Value>>
        let func_ref = func_rc.borrow(); // Ref<Value>

        // match via reference
        let (spec, body) = match &*func_ref {
            Value::Function { spec, body } => (spec.clone(), body.clone()),
            _ => panic!("expected function"),
        };

        // assign arguments
        Self::destruct_into(child_ctx.clone(), spec, args);

        // run the function body
        Self::block(child_ctx, body)
    }
}