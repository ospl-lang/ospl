//! THIS MODULE IS VIBE-CODED ENTIELY.
//! IT WAS WRITTEN BY A FUCKING CLANKER.
//! 
//! THANK YOU CHATGPT.
//! 
//! FOR REFERENCE LOOK AT THIS CONVERSATION I GUESS (I call it a lot of names.
//! FUCKING DIRTY CLANKER!):
//! https://chatgpt.com/c/68d84584-5b7c-8331-afc4-1c37c35a5002

use libffi::middle::{Arg, Cif, Type};
use std::rc::Rc;
use std::cell::RefCell;
use std::ffi::CString;
use std::mem;
use std::ptr;
use crate::ospl::Value;

struct Signature {
    // how to unpack the return value into OSPL args
    ret: Vec<Type>,

    // how to pack the OSPL arguments into C args
    arg: Vec<Type>
}

fn ospl_to_ffi_arg(val: &Value, cstrs: &mut Vec<CString>) -> Arg {
    return match val {
        Value::Byte(i) => Arg::new(&i),
        Value::SignedByte(i) => Arg::new(&i),
        Value::Word(i) => Arg::new(&i),
        Value::SignedWord(i) => Arg::new(&i),
        Value::DoubleWord(i) => Arg::new(&i),
        Value::SignedDoubleWord(i) => Arg::new(&i),
        Value::QuadrupleWord(i) => Arg::new(&i),
        Value::SignedQuadrupleWord(i) => Arg::new(&i),
        Value::Single(f) => Arg::new(&f),
        Value::Float(f) => Arg::new(&f),
        Value::String(s) => {
            let cstr = CString::new(s.clone()).unwrap();
            let ptr = cstr.as_ptr();
            cstrs.push(cstr);  // keep alive until call ends
            return Arg::new(&ptr)
        },
        Value::Ref(inner) => {
            let inner_ref = inner.borrow();
            let arg = ospl_to_ffi_arg(&inner_ref, cstrs);
            return Arg::new(&arg)
        },
        _ => panic!("unsupported OSPL type"),
    }
}

pub fn ospl_to_ffi_args(args: &[Value]) -> Vec<Arg> {
    let mut new_args = Vec::new();
    let mut strs = Vec::new();
    for arg in args {
        new_args.push(
            ospl_to_ffi_arg(arg, &mut strs)
        )
    }
    return new_args
}