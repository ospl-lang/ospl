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

pub unsafe fn write_value_to_memory(addr: usize, v: Rc<RefCell<Value>>) {
    unsafe {
        let v_ref = v.borrow();
        let ptr = addr as *mut u8;

        match &*v_ref {
            // POD primitives
            Value::Bool(b) => ptr.write(*b as u8),
            Value::Byte(b) => ptr.write(*b),
            Value::SignedByte(b) => ptr.write(*b as u8),
            Value::Word(w) => (ptr as *mut u16).write(*w),
            Value::SignedWord(w) => (ptr as *mut i16).write(*w),
            Value::DoubleWord(dw) => (ptr as *mut u32).write(*dw),
            Value::SignedDoubleWord(dw) => (ptr as *mut i32).write(*dw),
            Value::QuadrupleWord(qw) => (ptr as *mut u64).write(*qw),
            Value::SignedQuadrupleWord(qw) => (ptr as *mut i64).write(*qw),

            // very fast hopefully!
            Value::Tuple(elems) => {
                tuple_to_pod_fast(elems, ptr);  // writes everything in-place
            }

            _ => unreachable!(),
        }
    }
}


/// this is BAD...
pub unsafe fn write_flat_qword_to_memory(addr: usize, flat: &[u64]) {
    unsafe {
        core::ptr::copy_nonoverlapping(flat.as_ptr(), addr as *mut u64, flat.len());
    }
}

/// this function is needed for speed stuff.
/// 
/// short answer: I just said it
/// long answer:
/// 
/// okay so it's really slow to write one byte at a time, because the CPU cache
/// doesn't align. So instead we'll assume all values that we memcpy from an
/// OSPL tuple are `qword`s, which allows the (64-bit) CPU to work in (64-bit)
/// cache-sized chunks, increasing preformance by up to 8 times.
unsafe fn tuple_to_pod_fast(values: &[Rc<RefCell<Value>>], mut ptr: *mut u8) -> *mut u8 {
    unsafe {
        for i in 0..values.len() {
            let val = values.get_unchecked(i).as_ptr();

            match &*val {
                Value::Bool(b) => { ptr.write(*b as u8); ptr = ptr.add(1); }
                Value::Byte(b) => { ptr.write(*b); ptr = ptr.add(1); }
                Value::SignedByte(b) => { ptr.write(*b as u8); ptr = ptr.add(1); }
                Value::Word(w) => { (ptr as *mut u16).write(*w); ptr = ptr.add(2); }
                Value::SignedWord(w) => { (ptr as *mut i16).write(*w); ptr = ptr.add(2); }
                Value::DoubleWord(dw) => { (ptr as *mut u32).write(*dw); ptr = ptr.add(4); }
                Value::SignedDoubleWord(dw) => { (ptr as *mut i32).write(*dw); ptr = ptr.add(4); }
                Value::QuadrupleWord(qw) => { (ptr as *mut u64).write(*qw); ptr = ptr.add(8); }
                Value::SignedQuadrupleWord(qw) => { (ptr as *mut i64).write(*qw); ptr = ptr.add(8); }

                Value::Tuple(elems) => {
                    ptr = tuple_to_pod_fast(elems, ptr); // recurse, update pointer
                }

                _ => unreachable!(),
            }
        }

        return ptr  // return the pointer after the last written byte
    }
}

/* unsafe fn tuple_to_pod(values: &[Rc<RefCell<Value>>], out: &mut [u8]) {
    unsafe {
        for i in 0..values.len() {
            let val = values.get_unchecked(i).as_ptr();

            // this entire match gets optimized in release mode
            *out.get_unchecked_mut(i) = match *val {
                Value::QuadrupleWord(v) => v,
                _ => unreachable!(),  // optimized because of this
            };
        }
    }
} */