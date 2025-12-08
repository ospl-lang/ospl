use super::*;

use std::rc::{Rc, Weak};
use std::cell::RefCell;
use std::collections::HashMap;
use crate::ospl::ffi::FfiRegistry;

/// Context in which an AST is executed.
#[derive(Debug, Clone)]
pub struct Context {
    /// A reference to parent context.
    /// 
    /// This is needed so that you can climb up the chain to find variables
    /// and items above the current context.
    parent: Option<Weak<RefCell<Context>>>,

    /// A hashmap of symbols in this context. Self-explainatory
    pub vars: HashMap<String, Rc<RefCell<Value>>>,

    /// The current instance we're doing
    pub current_instance: Option<Rc<RefCell<Value>>>,

    /// Registry for FFI functions
    pub ffi_registry: FfiRegistry,
}

impl Context {
    pub fn new(parent: Option<Rc<RefCell<Context>>>) -> Self {
        let ffi_registry = parent
            .as_ref()
            .map(|p| p.borrow().ffi_registry.clone())
            .unwrap_or_else(FfiRegistry::new);

        Self {
            vars: HashMap::new(),
            parent: parent.as_ref().map(|p| Rc::downgrade(p)),
            current_instance: None,
            ffi_registry,
        }
    }

    pub fn get(&self, key: &str) -> Option<Rc<RefCell<Value>>> {
        if let Some(v) = self.vars.get(key) {
            // just a mental note: THIS CLONES AN RC
            // IT DOESN'T CLONE THE VALUE, IT JUST INCREMENTS THE REF COUNT
            return Some(v.clone())
        } else {
            // recurse deeply like I did to your mother
            return self.parent.as_ref()?.upgrade()?.borrow().get(key)
        }
    }

    /// Set a variable in a context
    /// 
    /// This function preforms three steps:
    /// 1. if the variable exists in the current context (`self`)
    ///    simply set it directly and return
    /// 2. traverse up the tree to try and find a parent context that does have it,
    ///    once (if) it's been found, simply set it directly and return
    /// 3. if the variable exists nowhere at all,
    ///    set it in the current context and return
    pub fn set(&mut self, key: &str, value: Value) {
        // Start with the current context
        let mut current_parent = self.parent.as_ref().and_then(|p| p.upgrade());

        // if the key exists locally, just update it
        if self.vars.contains_key(key) {
            self.vars.insert(key.to_string(), Rc::new(RefCell::new(value)));
            return;
        }

        // walk up the parent chain iteratively
        while let Some(parent_rc) = current_parent {
            let mut parent_ctx = parent_rc.borrow_mut();
            if parent_ctx.vars.contains_key(key) {
                parent_ctx.vars.insert(key.to_string(), Rc::new(RefCell::new(value)));
                return;
            }
            current_parent = parent_ctx.parent.as_ref().and_then(|p| p.upgrade());
        }

        // key wasn’t found anywhere, insert locally
        self.vars.insert(key.to_string(), Rc::new(RefCell::new(value)));
    }

    pub fn declare(&mut self, key: &str, value: Rc<RefCell<Value>>) {
        self.vars.insert(key.to_string(), value);
    }

    pub fn merge_with(&mut self, other: Context) {
        self.vars.extend(other.vars);
    }

    pub fn delete(&mut self, key: &str) {
        self.vars.remove(key);
    }
}
