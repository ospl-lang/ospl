use super::Value;
use std::cmp::Ordering;
use std::ops::SubAssign;
use std::{fmt::Display, ops::{Add, AddAssign, BitAnd, BitOr, BitXor, Div, Mul, Shl, Shr, Sub}, rc::Rc, cell::RefCell};

macro_rules! typical_op {
    ($lhs:expr, $rhs:expr, $op:tt) => {{
        // THIS IS NOT A PRETTY MATCH STATEMENT BUT IT WILL BE A MATCH
        // STATEMENT.
        match ($lhs, $rhs) {
            // integer types
            (Value::Byte(a), Value::Byte(b))                                => Value::Byte(a $op b),
            (Value::SignedByte(a), Value::SignedByte(b))                    => Value::SignedByte(a $op b),

            // more promotions
            (Value::Word(a), Value::Byte(b))                                => Value::Word(a $op b as u16),        // promote unsigned
            (Value::Word(a), Value::SignedByte(b))                          => Value::Word(a $op b as u16),        // promote unsigned
            (Value::SignedWord(a), Value::SignedByte(b))                    => Value::SignedWord(a $op b as i16),  // promote signed
            (Value::SignedWord(a), Value::Byte(b))                          => Value::SignedWord(a $op b as i16),  // promote signed
            (Value::Word(a), Value::Word(b))                                => Value::Word(a $op b),               // normal
            (Value::SignedWord(a), Value::SignedWord(b))                    => Value::SignedWord(a $op b),         // normal

            // A LOT MORE PROMOTIONS
            (Value::DoubleWord(a), Value::Byte(b))                          => Value::DoubleWord(a $op b as u32),        // promote unsigned b
            (Value::DoubleWord(a), Value::SignedByte(b))                    => Value::DoubleWord(a $op b as u32),        // promote unsigned b 
            (Value::DoubleWord(a), Value::Word(b))                          => Value::DoubleWord(a $op b as u32),        // promote unsigned w
            (Value::DoubleWord(a), Value::SignedWord(b))                    => Value::DoubleWord(a $op b as u32),        // promote unsigned w
            (Value::SignedDoubleWord(a), Value::Byte(b))                    => Value::SignedDoubleWord(a $op b as i32),  // promote signed b
            (Value::SignedDoubleWord(a), Value::SignedByte(b))              => Value::SignedDoubleWord(a $op b as i32),  // promote signed b 
            (Value::SignedDoubleWord(a), Value::Word(b))                    => Value::SignedDoubleWord(a $op b as i32),  // promote signed w
            (Value::SignedDoubleWord(a), Value::SignedWord(b))              => Value::SignedDoubleWord(a $op b as i32),  // promote signed w

            (Value::DoubleWord(a), Value::DoubleWord(b))                    => Value::DoubleWord(a $op b),         // normal
            (Value::SignedDoubleWord(a), Value::SignedDoubleWord(b))        => Value::SignedDoubleWord(a $op b),   // normal

            (Value::QuadrupleWord(a), Value::QuadrupleWord(b))              => Value::QuadrupleWord(a $op b),
            (Value::SignedQuadrupleWord(a), Value::SignedQuadrupleWord(b))  => Value::SignedQuadrupleWord(a $op b),

            // special one for pointer math

            // float types
            (Value::Half(a), Value::Half(b))                                => Value::Half(a $op b),
            (Value::Single(a), Value::Single(b))                            => Value::Single(a $op b),
            (Value::Float(a), Value::Float(b))                              => Value::Float(a $op b),

            // stuff you CANNOT do
            (Value::Null, _)                                                => panic!(">//< can't operate to Null!"),
            (_, Value::Null)                                                => panic!(">//< can't operate with Null!"),

            (Value::Void, _)                                                => panic!(">//< can't operate to Void!"),
            (_, Value::Void)                                                => panic!(">//< can't operate with Void!"),

            _ => panic!("I don't know how to do this operation on these types (possible type saftey issue)!")
        }
    }};
}

macro_rules! typical_op_assign {
    ($lhs:expr, $rhs:expr, $op:tt) => {{
        match ($lhs, $rhs) {
            // integer types
            (Value::Byte(a), Value::Byte(b))                                => *a $op b,
            (Value::SignedByte(a), Value::SignedByte(b))                    => *a $op b,
            (Value::Word(a), Value::Word(b))                                => *a $op b,
            (Value::SignedWord(a), Value::SignedWord(b))                    => *a $op b,
            (Value::DoubleWord(a), Value::DoubleWord(b))                    => *a $op b,
            (Value::SignedDoubleWord(a), Value::SignedDoubleWord(b))        => *a $op b,
            (Value::QuadrupleWord(a), Value::QuadrupleWord(b))              => *a $op b,
            (Value::SignedQuadrupleWord(a), Value::SignedQuadrupleWord(b))  => *a $op b,

            // float types
            (Value::Half(a), Value::Half(b))                                => *a $op b,
            (Value::Single(a), Value::Single(b))                            => *a $op b,
            (Value::Float(a), Value::Float(b))                              => *a $op b,

            // stuff you CANNOT do
            (Value::Null, _)                                                => panic!(">//< can't operate to Null!"),
            (_, Value::Null)                                                => panic!(">//< can't operate with Null!"),

            (Value::Void, _)                                                => panic!(">//< can't operate to Void!"),
            (_, Value::Void)                                                => panic!(">//< can't operate with Void!"),

            _ => panic!("I don't know how to do this operation on these types (possible type saftey issue)!")
        }
    }};
}

macro_rules! typical_cmp {
    ($lhs:expr, $rhs:expr, $op:tt) => {{
        match ($lhs, $rhs) {
            (Value::Byte(a), Value::Byte(b))                                => a $op b,
            (Value::SignedByte(a), Value::SignedByte(b))                    => a $op b,
            (Value::Word(a), Value::Word(b))                                => a $op b,
            (Value::SignedWord(a), Value::SignedWord(b))                    => a $op b,
            (Value::DoubleWord(a), Value::DoubleWord(b))                    => a $op b,
            (Value::SignedDoubleWord(a), Value::SignedDoubleWord(b))        => a $op b,
            (Value::QuadrupleWord(a), Value::QuadrupleWord(b))              => a $op b,
            (Value::SignedQuadrupleWord(a), Value::SignedQuadrupleWord(b))  => a $op b,
            (Value::Half(a), Value::Half(b))                                => a $op b,
            (Value::Single(a), Value::Single(b))                            => a $op b,
            (Value::Float(a), Value::Float(b))                              => a $op b,

            // non number types
            (Value::Tuple(a), Value::Tuple(b))                              => a $op b,
            (Value::String(a), Value::String(b))                            => a $op b,
            (Value::Bool(x), Value::Bool(y))                                => x $op y,

            // stuff you CANNOT do
            (Value::Null, Value::Null)                                      => true,

            (Value::Void, _)                                                => panic!(">//< can't compare to Void!"),
            (_, Value::Void)                                                => panic!(">//< can't compare with Void!"),

            _                                                               => panic!(">//< can't compare {:?} with {:?}", $lhs, $rhs),
        }
    }};
}

macro_rules! bit_op {
    ($lhs:expr, $rhs:expr, $op:tt) => {
        match ($lhs, $rhs) {
            (Value::Byte(a), Value::Byte(b))                              => Value::Byte(a $op b),
            (Value::SignedByte(a), Value::SignedByte(b))                  => Value::SignedByte(a $op b),
            (Value::Word(a), Value::Word(b))                              => Value::Word(a $op b),
            (Value::SignedWord(a), Value::SignedWord(b))                  => Value::SignedWord(a $op b),
            (Value::DoubleWord(a), Value::DoubleWord(b))                  => Value::DoubleWord(a $op b),
            (Value::SignedDoubleWord(a), Value::SignedDoubleWord(b))      => Value::SignedDoubleWord(a $op b),
            (Value::QuadrupleWord(a), Value::QuadrupleWord(b))            => Value::QuadrupleWord(a $op b),
            (Value::SignedQuadrupleWord(a), Value::SignedQuadrupleWord(b))=> Value::SignedQuadrupleWord(a $op b),
            _ => panic!("i dont know WHAT the fuck you're doing, but you shouldn't be doing it"),
        }
    };
}

impl BitAnd for Value {
    type Output = Self;
    fn bitand(self, rhs: Self) -> Self::Output {
        return bit_op!(&self, &rhs, &)
    }
}

impl BitOr for Value {
    type Output = Self;
    fn bitor(self, rhs: Self) -> Self::Output {
        return bit_op!(&self, &rhs, |)
    }
}

impl BitXor for Value {
    type Output = Self;
    fn bitxor(self, rhs: Self) -> Self::Output {
        return bit_op!(&self, &rhs, ^)
    }
}

impl Shl for Value {
    type Output = Self;
    fn shl(self, rhs: Self) -> Self::Output {
        return bit_op!(&self, &rhs, <<)
    }
}

impl Shr for Value {
    type Output = Self;
    fn shr(self, rhs: Self) -> Self::Output {
        return bit_op!(&self, &rhs, >>)
    }
}

impl Eq for Value {}
impl PartialEq for Value {
    fn eq(&self, other: &Self) -> bool { typical_cmp!(self, other, ==) }
    fn ne(&self, other: &Self) -> bool { typical_cmp!(self, other, !=) }
}

impl PartialOrd for Value {
    fn partial_cmp(&self, _: &Self) -> Option<Ordering> {
        unreachable!("apparently you broke the parser so bad it preformed an invalid comparison");
    }

    fn le(&self, other: &Self) -> bool { typical_cmp!(self, other, <=) }
    fn lt(&self, other: &Self) -> bool { typical_cmp!(self, other, <) }
    fn gt(&self, other: &Self) -> bool { typical_cmp!(self, other, >) }
    fn ge(&self, other: &Self) -> bool { typical_cmp!(self, other, >=) }
}

impl Add for Value {
    type Output = Self;
    fn add(self, rhs: Self) -> Self::Output {
        return match (&self, &rhs) {
            // append chars to string, might not be needed because typical_op
            // might handle it correctly already, idk.
            (Self::String(a), Self::String(b)) => {
                let mut s = a.to_owned();
                s.push_str(b.as_str());
                Self::String(s)
            },
            
            // append to tuple
            (Self::Tuple(a), b @ _) => Self::Tuple({
                let mut cln = a.clone();
                
                cln.push(
                    Rc::new(
                        RefCell::new(
                            b.clone()
                        )
                    )
                );
                
                cln
            }),
            _ => typical_op!(self, rhs, +)
        }
    }
}

// incomplete but I can't be fucked
impl AddAssign for Value {
    fn add_assign(&mut self, rhs: Self) {
        match (self, rhs) {
            (Self::String(a), Self::String(b)) => {
                a.push_str(&b);
            },
            (Self::Tuple(a), b) => {
                a.push(Rc::new(RefCell::new(b)));
            },
            (s, r) => typical_op_assign!(s, r, +=),
        }
    }
}

impl Sub for Value {
    type Output = Self;
    fn sub(self, rhs: Self) -> Self::Output {
        return match (&self, &rhs) {
            // pop N chars from string
            (Self::String(a), Self::Byte(n)) => Self::String(
                pop_n_chars_of_string_fast(
                    // clone
                    &mut a.to_owned(),
                    *n as usize
                )
            ),
            (Self::String(a), Self::Word(n)) => Self::String(pop_n_chars_of_string_fast(&mut a.to_owned(), *n as usize)),
            (Self::String(a), Self::DoubleWord(n)) => Self::String(pop_n_chars_of_string_fast(&mut a.to_owned(), *n as usize)),
            (Self::String(a), Self::QuadrupleWord(n)) => Self::String(pop_n_chars_of_string_fast(&mut a.to_owned(), *n as usize)),
            _ => typical_op!(self, rhs, -)
        }
    }
}

fn pop_n_chars_of_string_fast(a: &mut String, n: usize) -> String {
    let mut s = a.to_owned();
    let new_len = s.char_indices()
        .rev()
        .nth(n)
        .map(|(idx, _)| idx)
        .unwrap_or(0);

    s.truncate(new_len);
    return s
}

fn pop_n_chars_of_string(a: &mut String, mut b: usize) {
    while b > 0 {
        a.pop();
        b -= 1;
    };
}

fn pop_n_elems_of_tup(a: &mut Vec<Rc<RefCell<Value>>>, mut b: usize) {
    while b > 0 {
        a.pop();
        b -= 1;
    };
}

impl SubAssign for Value {
    fn sub_assign(&mut self, rhs: Self) {
        match (self, rhs) {
            // ensure that we can't pop off a negative amount of chars, only
            // allow unsigned numbers. TODO: This is slow!
            (Self::String(a), Self::Byte(b)) => pop_n_chars_of_string(a, b as usize),
            (Self::String(a), Self::Word(b)) => pop_n_chars_of_string(a, b as usize),
            (Self::String(a), Self::DoubleWord(b)) => pop_n_chars_of_string(a, b as usize),
            (Self::String(a), Self::QuadrupleWord(b)) => pop_n_chars_of_string(a, b as usize),

            // tuple stuff agh
            (Self::Tuple(a), Self::Byte(b)) => pop_n_elems_of_tup(a, b as usize),
            (Self::Tuple(a), Self::Word(b)) => pop_n_elems_of_tup(a, b as usize),
            (Self::Tuple(a), Self::DoubleWord(b)) => pop_n_elems_of_tup(a, b as usize),
            (Self::Tuple(a), Self::QuadrupleWord(b)) => pop_n_elems_of_tup(a, b as usize),
            
            // try this then
            (s, r) => typical_op_assign!(s, r, -=),
        };
    }
}

impl Div for Value {
    type Output = Self;
    fn div(self, rhs: Self) -> Self::Output {
        return typical_op!(self, rhs, /)
    }
}

impl Mul for Value {
    type Output = Self;
    fn mul(self, rhs: Self) -> Self::Output {
        return typical_op!(self, rhs, *)
    }
}

impl Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let thing_to_write = match self {
            // bools
            Self::Bool(b) => if *b { "true" } else { "false" },

            // numbers
            Self::Byte(n) => &n.to_string(),
            Self::SignedByte(n) => &n.to_string(),
            Self::Word(n) => &n.to_string(),
            Self::SignedWord(n) => &n.to_string(),
            Self::DoubleWord(n) => &n.to_string(),
            Self::SignedDoubleWord(n) => &n.to_string(),
            Self::QuadrupleWord(n) => &n.to_string(),
            Self::SignedQuadrupleWord(n) => &n.to_string(),
            Self::Half(n) => &n.to_string(),
            Self::Single(n) => &n.to_string(),
            Self::Float(n) => &n.to_string(),

            Self::Null => "null",
            Self::Void => "void",

            // DO NOT USE TO_STRING OR YOU WILL BLOW YOUR STACK
            Self::String(s) => s,

            // try to print debug
            other @ _ => &format!("{:#?}", other),
        };
        write!(f, "{}", thing_to_write)
    }
}