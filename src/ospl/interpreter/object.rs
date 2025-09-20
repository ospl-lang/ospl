use std::rc::Rc;

use super::*;

impl Interpreter {
    /// construct a class
    /// 
    /// this function operates in three steps:
    /// 1. construct each parent class (if any)
    /// 2. merge all properties into the new object
    /// 3. call the class' `_init` function
    pub fn class_construct(ctx: Rc<RefCell<Context>>, cls: Rc<RefCell<Value>>) -> Rc<RefCell<Value>> {
        // Start with an empty object
        let obj = Rc::new(
            RefCell::new(
                Value::Object {
                    symbols: HashMap::new(),
                }
            )
        );

        // Get parents
        let Value::Class { parents, .. } = &*cls.borrow() else {
            panic!("can't construct non-class {:#?}", cls);
        };

        // Merge parent properties
        if let Value::Object { symbols: current_hm, ..} = &mut *obj.borrow_mut() {
            for parent in parents {
                if let Value::Object {symbols: parent_hm, ..} = &*parent.borrow() {
                    current_hm.extend(parent_hm.clone());  // shallow clone of Values
                }
            }
        }

        // Call the _init function if it exists
        if let Value::Object { symbols: obj_hm, .. } = &*obj.borrow() {
            if let Some(init_fn) = obj_hm.get("_init") {
                // Pass obj so _init can mutate it
                let _ = Self::do_function_call(
                    Some(ctx.clone()),
                    init_fn.clone(),
                    vec![obj.clone()]
                );
            }
        }

        return obj
    }
}
