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
    ) -> Result<(), ()> {
        // should ALWAYS be true at runtime, otherwise someone fucked it up.
        if spec.len() != args.len() {
            return Err(())
        }

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
                )?,
                Subspec::Ignore => {},
                Subspec::Literal(v) => if *arg.borrow() != v {
                    return Err(());
                },
                Subspec::ThisRef(id) => {
                    // this is ugly but we have to do it
                    // to make the borrow checker happy
                    let mut b = ctx.borrow_mut();

                    let thing = b
                        .current_instance
                        .as_ref()
                        .expect("agh")
                        .clone();  // clone or use `take()` if you want to move it

                    b.set(&id, Value::Ref(thing));  // operate with the same mutable borrow
                }
            };
        }
        return Ok(());
    }

    pub fn do_call(
        ctx: Option<Rc<RefCell<Context>>>,
        f: Rc<RefCell<Value>>,
        args: Vec<Rc<RefCell<Value>>>
    ) -> Option<Rc<RefCell<Value>>> {
        match *f.borrow() {
            Value::RealFn {..} => return Self::do_fn_call(f.clone(), args),
            Value::MacroFn {..} => return Self::do_macro_call(ctx, f.clone(), args),
            _ => panic!("tried to call an uncallable value")
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
    pub fn do_macro_call(
        ctx: Option<Rc<RefCell<Context>>>,
        f: Rc<RefCell<Value>>,
        args: Vec<Rc<RefCell<Value>>>
    ) -> Option<Rc<RefCell<Value>>> {
        // create child context
        let child_ctx = Rc::new(
            RefCell::new(
                Context::new(
                    ctx
                )
            )
        );

        // match via reference
        let f_ref = f.borrow(); // keep Ref<Value> alive
        let (spec, body) = match &*f_ref {
            Value::MacroFn { spec, body } => (spec, body),
            _ => panic!(">//< tried to call non-macro: {:#?}!", f_ref),
        };

        // assign arguments
        // cloning spec is not cheap, I don't like it but whatever
        Self::destruct_into(child_ctx.clone(), spec.clone(), args).ok()?;

        // run the function body
        return Self::block(child_ctx, body.clone());
    }

    pub fn do_fn_call(
        f: Rc<RefCell<Value>>,
        args: Vec<Rc<RefCell<Value>>>
    ) -> Option<Rc<RefCell<Value>>> {
        let f_ref = f.borrow();
        let Value::RealFn { spec, body , ctx} = &*f_ref
        else {
            panic!("tried to call non-function")
        };

        // create child context
        let child_ctx = Rc::new(
            RefCell::new(
                Context::new(
                    Some(
                        ctx.clone()
                    )
                )
            )
        );

        // cloning spec is not cheap, I don't like it but whatever
        Self::destruct_into(child_ctx.clone(), spec.clone(), args).ok()?;

        // run the function body
        return Self::block(child_ctx, body.clone());
    }
}