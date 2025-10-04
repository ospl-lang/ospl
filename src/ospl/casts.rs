use std::cell::RefCell;
use std::rc::Rc;
use std::slice;

use crate::Value;

// strings
pub unsafe fn ptr_to_string(ptr: *const u8) -> Value {
    unsafe {
        // get size
        let len = strlen(ptr);
        let slice = slice::from_raw_parts(ptr, len);  // &[u8]
        let vec = slice.to_vec();  // Vec<u8>
        return Value::String(
            String::from_utf8(vec).expect("failed to cast pointer to string: invalid UTF-8")
        )
    }
}

unsafe fn strlen(ptr: *const u8) -> usize {
    unsafe {
        let mut len = 0;
        while ptr.add(len).read() != 0 {
            len += 1;
        }
        return len
    }
}

// tuples
pub unsafe fn ptr_to_tuple(ptr: *const u8) -> Value {
    unsafe {
        // get size
        let len = strlen(ptr);
        let slice = slice::from_raw_parts(ptr, len);  // &[u8]
        let vec = slice.to_vec();  // Vec<u8>
        let new_vec =
            vec
            .iter()
            .map(|x| Rc::new(RefCell::new(Value::Byte(*x))))
            .collect();

        return Value::Tuple(new_vec)
    }
}
