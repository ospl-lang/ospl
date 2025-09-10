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
    pub fn destruct_into(
        ctx: Rc<RefCell<Context>>,
        spec: Vec<Subspec>,
        args: Vec<Rc<RefCell<Value>>>
    ) -> () {
        // should ALWAYS be true at runtime, otherwise someone fucked it up.
        assert!(spec.len() == args.len());

        for (subspec, arg) in spec.into_iter().zip(args.into_iter()) {
            match subspec {
                Subspec::BindRef(key) => {
                    ctx.borrow_mut().set(&key, 
                        // this passes by reference
                        arg
                        .borrow()
                        .clone()
                    );
                },
                Subspec::Bind(key) => {
                    ctx.borrow_mut().set(&key, 
                        // OSPL passes by value by default.
                        arg
                        .borrow()
                        .deep_clone()
                    );
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
        f: Rc<RefCell<Value>>,
        args: Vec<Rc<RefCell<Value>>>
    ) -> Option<Rc<RefCell<Value>>> {
        // create child context
        let child_ctx = Rc::new(
            RefCell::new(
                Context::new(Some(ctx.clone()))));

        // match via reference
        let f_ref = f.borrow(); // keep Ref<Value> alive
        let (spec, body) = match &*f_ref {
            Value::Function { spec, body } => (spec, body),
            _ => panic!(">//< tried to call non-function: {:#?}!", f_ref),
        };

        // assign arguments
        // cloning spec is not cheap, I don't like it but whatever
        Self::destruct_into(child_ctx.clone(), spec.clone(), args);

        // run the function body
        Self::block(child_ctx, body.clone())
    }
}