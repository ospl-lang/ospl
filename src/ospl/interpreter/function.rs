use super::*;

use std::rc::Rc;
use std::cell::RefCell;

#[derive(Debug)]
pub enum DestructionError {
    NotEnoughArgs,
    TooManyArgs,
    LiteralRequirementFailed,
    ThisRefShouldNotBeThere,
    BindTypedCheckFailed {
        expected: Type,
        got: Type
    }
}

#[derive(Debug)]
pub enum CallError {
    DestructionError(DestructionError),
    IlligalType,
    WhydYouCallIt,
    LexicalScopeDeleted
}

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
        spec: &Vec<Subspec>,
        arg_list: &Vec<Rc<RefCell<Value>>>
    ) -> Result<(), DestructionError> {
        // TODO: make this error() too...
        // can be done by putting the last executed `SpannedStatement` into Context, although I particularly don't like that strategy... or make `Interpreter` store it...
        // or just pass it into the function, whatever works... or just never do this one
        let mut args = arg_list.into_iter();

        for subspec in spec.into_iter() {
            match subspec {
                Subspec::ThisRef(id) => {
                    // this is ugly but we have to do it
                    // to make the borrow checker happy
                    let mut b = ctx.borrow_mut();

                    let this =
                        b
                        .current_instance
                        .as_ref()
                        .ok_or(DestructionError::ThisRefShouldNotBeThere)?
                        .clone();

                    b.set(&id, Value::Ref(this));  // operate with the same mutable borrow

                    /* we want to UNSET the current_instance after any
                     * destructions, we don't want that to linger around.
                     * 
                     * THIS IS A VERY, EXTREMELY RETARDED WAY OF DOING THIS!
                     * the "elegant" solution is to create a new context for
                     * every property access, with current_instance being set
                     * accordingly, but that is a lot of allocations, and
                     * allocs don't grow on trees (unfortunately...)
                     */
                    b.current_instance.take();
                },

                // nobody uses these but whatever
                Subspec::BindRef(key) => {
                    ctx.borrow_mut().set(&key, 
                        // this passes by reference
                        // this is retarded
                        args.next().ok_or(DestructionError::NotEnoughArgs)?
                        .borrow()
                        .clone()
                    );
                },

                Subspec::Bind(key) => {
                    ctx.borrow_mut().set(&key, 
                        // OSPL passes by value by default.
                        args.next().ok_or(DestructionError::NotEnoughArgs)?
                        .borrow()
                        .deep_clone()
                    );
                },

                // these are slower but allow type enforcement at runtime
                // seperare variants for speed reasons
                Subspec::BindRefTyped(key, target_typ) => {
                    let data = 
                        args
                        .next()
                        .ok_or(DestructionError::NotEnoughArgs)?
                        .borrow()
                        .clone();  // difference!

                    let typ = data.as_type();
                    if typ != *target_typ {
                        panic!("failed type annotation check on BindRefTyped, expected {:?}, got {:?}", target_typ, typ);
                    }

                    ctx.borrow_mut().set(&key, data);
                },

                Subspec::BindTyped(key, target_typ) => {
                    let data = 
                        args
                        .next()
                        .ok_or(DestructionError::NotEnoughArgs)?
                        .borrow()
                        .deep_clone();

                    let typ = data.as_type();
                    if typ != *target_typ {
                        return Err(DestructionError::BindTypedCheckFailed {
                            expected: typ,
                            got: target_typ.clone()  // clone is bad but idfc
                        })
                    }

                    ctx.borrow_mut().set(&key, data);
                },

                Subspec::Destruct(tree) => Self::destruct_into(
                    ctx.clone(),
                    tree,
                    &args
                        .next()
                        .ok_or(DestructionError::NotEnoughArgs)?
                        .borrow()
                        .as_values()
                )?,

                Subspec::LiteralRequirement(v) => {
                    let arg = args.next().ok_or(DestructionError::NotEnoughArgs)?;
                    let value = &*Self::expr(ctx.clone(), v);
                    if *arg.borrow() != *value.borrow() {
                        return Err(DestructionError::LiteralRequirementFailed);
                    }
                }

                Subspec::Ignore => {}
            };
        }

        // make sure we don't have too many args
        if args.next().is_some() {
            return Err(DestructionError::TooManyArgs);
        }

        return Ok(());
    }

    pub fn do_call(
        ctx: Option<Rc<RefCell<Context>>>,
        f: Rc<RefCell<Value>>,
        args: Vec<Rc<RefCell<Value>>>
    ) -> Result<Option<Rc<RefCell<Value>>>, CallError> {
        match *f.borrow() {
            Value::RealFn {..} => return Self::do_fn_call(ctx, f.clone(), args),
            Value::MacroFn {..} => return Self::do_macro_call(ctx, f.clone(), args),
            _ => Err(CallError::WhydYouCallIt)
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
    ) -> Result<Option<Rc<RefCell<Value>>>, CallError> {
        // create child context
        let child_ctx = Rc::new(
            RefCell::new(
                Context::new(
                    ctx.clone()
                )
            )
        );

        // this shit is fucking retarded but I don't care I just wanna ship
        // the damn language at this fucking point.
        if let Some(parent_ctx) = ctx {
            child_ctx.borrow_mut().current_instance = parent_ctx.borrow().current_instance.clone();
        }

        // match via reference
        let f_ref = f.borrow(); // keep Ref<Value> alive
        let (spec, body) = match &*f_ref {
            Value::MacroFn { spec, body } => (spec, body),
            _ => return Err(CallError::IlligalType)
        };

        // assign arguments
        Self::destruct_into(child_ctx.clone(), spec, &args)
            .map_err(|e| return CallError::DestructionError(e))?;

        // run the function body
        return Ok(Self::block(child_ctx, body));
    }

    pub fn do_fn_call(
        immediate_ctx: Option<Rc<RefCell<Context>>>,
        f: Rc<RefCell<Value>>,
        args: Vec<Rc<RefCell<Value>>>,
    ) -> Result<Option<Rc<RefCell<Value>>>, CallError> {
        let f_ref = f.borrow();
        let Value::RealFn { spec, body , ctx} = &*f_ref
        else {
            return Err(CallError::IlligalType)
        };

        // create child context
        let child_ctx = Rc::new(
            RefCell::new(
                Context::new(Some(ctx.clone()))
            )
        );

        // this shit is fucking retarded but I don't care I just wanna ship
        // the damn language at this fucking point.

        // here we need the call's context otherwise the entire thing has a stroke
        if let Some(rc_refcell) = immediate_ctx {
            let immediate = rc_refcell.borrow();
            child_ctx.borrow_mut().current_instance = immediate.current_instance.clone();
        }

        // look at me ma, no clone!
        Self::destruct_into(child_ctx.clone(), spec, &args)
            .map_err(|e| return CallError::DestructionError(e))?;

        // run the function body
        return Ok(Self::block(child_ctx, body));
    }
}
