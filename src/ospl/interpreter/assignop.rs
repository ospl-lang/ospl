//! TODO: make me not want to throw up after seeing this module...
use super::*;

impl Interpreter {
    pub fn handle_string_minus_string(s: &mut String, pat: &str) {
        if let Some(pos) = s.find(pat) {
            let end = pos + pat.len();
            s.replace_range(pos..end, "");  // remove the range in place
        }
    }

    pub fn handle_add_assign(mut v: std::cell::RefMut<'_, Value>, x: std::cell::Ref<'_, Value>) {
        match (&mut *v, &*x) {
            (Value::String(s), Value::String(s2)) => s.push_str(s2),

            // numbers
            (Value::Byte(a), Value::Byte(b)) => *a += b,
            (Value::SignedByte(a), Value::SignedByte(b)) => *a += b,
            (Value::Word(a), Value::Word(b)) => *a += b,
            (Value::SignedWord(a), Value::SignedWord(b)) => *a += b,
            (Value::DoubleWord(a), Value::DoubleWord(b)) => *a += b,
            (Value::SignedDoubleWord(a), Value::SignedDoubleWord(b)) => *a += b,
            (Value::QuadrupleWord(a), Value::QuadrupleWord(b)) => *a += b,
            (Value::SignedQuadrupleWord(a), Value::SignedQuadrupleWord(b)) => *a += b,
            (Value::Half(a), Value::Half(b)) => *a += b,
            (Value::Float(a), Value::Float(b)) => *a += b,
            (Value::Single(a), Value::Single(b)) => *a += b,

            // TODO: FIX THIS FUCKING ERROR MESSAGE
            _ => unimplemented!("idfk how to do {:#?} += {:#?}", v, x)
        }
    }

    pub fn handle_sub_assign(mut v: std::cell::RefMut<'_, Value>, x: std::cell::Ref<'_, Value>) {
        match (&mut *v, &*x) {
            (Value::String(s), Value::QuadrupleWord(new_len)) => s.truncate(*new_len as usize),
            (Value::String(s), Value::String(pat)) => Self::handle_string_minus_string(s, &pat),

            // numbers
            (Value::Byte(a), Value::Byte(b)) => *a -= b,
            (Value::SignedByte(a), Value::SignedByte(b)) => *a -= b,
            (Value::Word(a), Value::Word(b)) => *a -= b,
            (Value::SignedWord(a), Value::SignedWord(b)) => *a -= b,
            (Value::DoubleWord(a), Value::DoubleWord(b)) => *a -= b,
            (Value::SignedDoubleWord(a), Value::SignedDoubleWord(b)) => *a -= b,
            (Value::QuadrupleWord(a), Value::QuadrupleWord(b)) => *a -= b,
            (Value::SignedQuadrupleWord(a), Value::SignedQuadrupleWord(b)) => *a -= b,
            (Value::Half(a), Value::Half(b)) => *a -= b,
            (Value::Float(a), Value::Float(b)) => *a -= b,
            (Value::Single(a), Value::Single(b)) => *a -= b,

            // TODO: FIX THIS FUCKING ERROR MESSAGE
            _ => unimplemented!("idfk how to do {:#?} -= {:#?}", v, x)
        }
    }
}