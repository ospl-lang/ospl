use super::*;

impl Interpreter {
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
    pub fn do_loop(
        ctx: Rc<RefCell<Context>>,
        body: Block
    ) -> StatementControl {
        'outer: loop {
            for stmt in &body.stmts {   // iterate by reference to avoid cloning
                let result = Self::stmt(ctx.clone(), stmt.clone());
                match result {
                    StatementControl::Break => return result,
                    StatementControl::EarlyReturn(_) => return result,
                    StatementControl::Continue => continue 'outer,  // continue loop properly
                    _ => {}  // do nothing, just move to next stmt
                }
            }
        }
    }

    /// Execute a `check` statement
    /// 
    /// `check` may match multiple patterns,
    /// `check` may be broken out of using `break`
    /// `check` may be continued too
    pub fn preform_check(
        ctx: Rc<RefCell<Context>>,
        matching: Box<Expr>,
        cases: Vec<(Vec<Subspec>, Block)>
    ) -> StatementControl {
        let thing = Self::expr(ctx.clone(), *matching);
        'outer: for (spec, ex) in cases {
            // create a new context for this case
            // this could very well be optimized... but that's what
            // `select` is for!
            let newctx = Rc::new(
                RefCell::new(
                    Context::new(
                        Some(
                            ctx.clone()
                        )
                    )
                )
            );

            // try to destruct into this new context
            if let Ok(_) = Self::destruct_into(newctx.clone(), spec, thing.borrow().as_values()) {
                // if it's successful then run this block
                for stmt in ex.stmts {
                    let ctrl = Self::stmt(newctx.clone(), stmt);
                    match ctrl {
                        StatementControl::Break => break 'outer,
                        StatementControl::EarlyReturn(_) => return ctrl,
                        StatementControl::Continue => continue 'outer,
                        _ => {},
                    }
                }
            } else {
                continue;
            }
        }

        return StatementControl::Default;
    }

    pub fn preform_select(
        ctx: Rc<RefCell<Context>>,
        matching: Box<Expr>,
        cases: Vec<(Vec<Subspec>, Block)>
    ) -> StatementControl {
        let thing = Self::expr(ctx.clone(), *matching);
        // create a new context for our match
        // this could very well be optimized... but that's what
        let newctx = Rc::new(
            RefCell::new(
                Context::new(
                    Some(
                        ctx.clone()
                    )
                )
            )
        );

        'outer: for (spec, ex) in cases {
            // try to destruct into this new context
            if let Ok(_) = Self::destruct_into(newctx.clone(), spec, thing.borrow().as_values()) {
                // if it's successful then run this block
                for stmt in ex.stmts {
                    let ctrl = Self::stmt(newctx.clone(), stmt);
                    match ctrl {
                        StatementControl::Break => break 'outer,
                        StatementControl::EarlyReturn(_) => return ctrl,
                        StatementControl::Continue => continue 'outer,
                        _ => {},
                    }
                }
                
                // then we return
                return StatementControl::Default;
            } else {
                continue;
            }
        }

        return StatementControl::Default;
    }
}