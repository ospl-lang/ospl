use std::rc::Rc;

use super::*;

impl Interpreter {
    /// construct a class
    /// 
    /// this function operates in three steps:
    /// 1. construct each parent class (if any)
    /// 2. merge all properties into the new object
    /// 3. call the class' `_init` function
    pub fn class_construct(_ctx: Rc<RefCell<Context>>, cls: Rc<RefCell<Value>>) -> Rc<RefCell<Value>> {
        // === NEW BS ===
        let Value::Class { symbols, .. } = &*cls.borrow() else {
            panic!("uhh my brain is turned off idk what the panic should be")
        };
        let mut obj = Value::Object { symbols: symbols.clone() };

        // === OLD CODE, MIGHT NOT WORK ===
        /*
         * Note: there's a bug where if this function implements the above, and
         * some EXTREMELY fucking specific shit happens, the program will panic
         * on this line in mod.rs
         * ```
         * Statement::Assign { left, right } => {
         *      let var: Rc<RefCell<Value>> = Self::expr(ctx.clone(), *left);
         *      let lit: Rc<RefCell<Value>> = Self::expr(ctx.clone(), *right);
         * 
         *      *var.borrow_mut() = lit.borrow().clone();  // here
         *      return StatementControl::Default
         *  },
         * ```
         * with a `RefCell already borrowed` error.
         * TODO: find exact cause and fix.
        */

        // Get parents
        let Value::Class { parents, .. } = &*cls.borrow() else {
            panic!("can't construct non-class {:#?}", cls);
        };

        if let Value::Object { symbols: current_hm, .. } = &mut obj {
            // Merge parent properties
            for parent in parents {
                if let Value::Object { symbols: parent_hm, .. } = &*parent.borrow() {
                    current_hm.extend(parent_hm.clone());  // shallow clone of Values
                }
            }
        }

        return Rc::new(RefCell::new(obj));
    }
}
