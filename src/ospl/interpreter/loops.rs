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
    pub fn do_loop(ctx: Rc<RefCell<Context>>, body: Block) -> StatementControl {
        'outer: loop {
            for stmt in &body.0 {   // iterate by reference to avoid cloning
                let result = Self::stmt(ctx.clone(), stmt.clone());
                match result {
                    StatementControl::Break(_) => return result,
                    StatementControl::EarlyReturn(_) => return result,
                    StatementControl::Continue => continue 'outer,  // continue loop properly
                    _ => {}  // do nothing, just move to next stmt
                }
            }
        }
    }
}