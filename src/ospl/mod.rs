use std::{
    collections::HashMap,
    ops::{Add, Div, Mul, Sub},
};

/// a function spec
#[derive(Debug, Clone)]
pub enum Subspec {
    Bind(String),
    Ignore,
    Destruct(Vec<Subspec>),
    Rest(String),
}

pub mod interpreter;

///////////////////////////////////////////////////////////////////////////////
// values and types
///////////////////////////////////////////////////////////////////////////////

/// Represents a value in OSPL
#[derive(Debug, Clone)]
pub enum Value {
    // stable OSPL types
    Ref(Box<Value>),
    Null,
    Void,

    // integers
    Byte(u8),
    SignedByte(i8),
    Word(u16),
    SignedWord(i16),
    DoubleWord(u32),
    SignedDoubleWord(i32),
    QuadrupleWord(u64),
    SignedQuadruleWord(i64),

    // can't actually use f16, as it is unstable.
    Half(f32),
    Single(f32),
    Float(f64),

    Bool(bool),
    String(String),
    Tuple(Vec<Value>),
    Mixmap {
        ordered: Vec<Value>,
        keyed: HashMap<String, Value>,
    },
    Function {
        spec: Vec<Subspec>,
        body: Block,
    },
}

impl Value {
    pub fn into_values(self) -> Vec<Value> {
        return match self {
            Self::Tuple(vals) => vals,
            Self::Mixmap { ordered, .. } => ordered,
            _ => vec![self],
        }
    }

    pub fn is_integer(&self) -> bool {
        return matches!(self,
            Self::Byte(_) |
            Self::SignedByte(_) |
            Self::Word(_) |
            Self::SignedWord(_) |
            Self::DoubleWord(_) |
            Self::SignedDoubleWord(_) |
            Self::QuadrupleWord(_) |
            Self::SignedQuadruleWord(_)
        )
    }

    pub fn is_float(&self) -> bool {
        return matches!(self,
            Self::Float(_) |
            Self::Single(_) |
            Self::Half(_)
        )
    }

    pub fn is_number(&self) -> bool {
        return self.is_float() | self.is_integer()
    }

    pub fn truthiness(&self) -> bool {
        match self {
            Value::Null | Value::Void => false,

            Value::Byte(n) => *n != 0,
            Value::SignedByte(n) => *n != 0,
            Value::Word(n) => *n != 0,
            Value::SignedWord(n) => *n != 0,
            Value::DoubleWord(n) => *n != 0,
            Value::SignedDoubleWord(n) => *n != 0,
            Value::QuadrupleWord(n) => *n != 0,
            Value::SignedQuadruleWord(n) => *n != 0,

            Value::Half(n) => *n != 0.0,
            Value::Single(n) => *n != 0.0,
            Value::Float(n) => *n != 0.0,

            Value::Bool(b) => *b,

            Value::String(s) => !s.is_empty(),
            Value::Tuple(t) => !t.is_empty(),

            _ => true
        }
    }
}

macro_rules! typical_op {
    ($lhs:expr, $rhs:expr, $op:tt) => {{
        match ($lhs, $rhs) {
            // integer types
            (Value::Byte(a), Value::Byte(b))                                => Value::Byte(a $op b),
            (Value::SignedByte(a), Value::SignedByte(b))                    => Value::SignedByte(a $op b),
            (Value::Word(a), Value::Word(b))                                => Value::Word(a $op b),
            (Value::SignedWord(a), Value::SignedWord(b))                    => Value::SignedWord(a $op b),
            (Value::DoubleWord(a), Value::DoubleWord(b))                    => Value::DoubleWord(a $op b),
            (Value::SignedDoubleWord(a), Value::SignedDoubleWord(b))        => Value::SignedDoubleWord(a $op b),
            (Value::QuadrupleWord(a), Value::QuadrupleWord(b))              => Value::QuadrupleWord(a $op b),
            (Value::SignedQuadruleWord(a), Value::SignedQuadruleWord(b))    => Value::SignedQuadruleWord(a $op b),
            (Value::Half(a), Value::Half(b))                                => Value::Half(a $op b),
            (Value::Single(a), Value::Single(b))                            => Value::Single(a $op b),
            (Value::Float(a), Value::Float(b))                              => Value::Float(a $op b),

            // stuff you CANNOT do
            (Value::Null, _)                                                => panic!(">//< can't operate to Null!"),
            (_, Value::Null)                                                => panic!(">//< can't operate with Null!"),

            (Value::Void, _)                                                => panic!(">//< can't operate to Void!"),
            (_, Value::Void)                                                => panic!(">//< can't operate with Void!"),

            _ => panic!(">//< I don't know how to do this operation!")
        }
    }};
}

macro_rules! typical_cmp {
    ($lhs:expr, $rhs:expr, $op:tt) => {{
        match ($lhs, $rhs) {
            (Value::Byte(a), Value::Byte(b)) => a $op b,
            (Value::SignedByte(a), Value::SignedByte(b)) => a $op b,
            (Value::Word(a), Value::Word(b)) => a $op b,
            (Value::SignedWord(a), Value::SignedWord(b)) => a $op b,
            (Value::DoubleWord(a), Value::DoubleWord(b)) => a $op b,
            (Value::SignedDoubleWord(a), Value::SignedDoubleWord(b)) => a $op b,
            (Value::QuadrupleWord(a), Value::QuadrupleWord(b)) => a $op b,
            (Value::SignedQuadruleWord(a), Value::SignedQuadruleWord(b)) => a $op b,
            (Value::Half(a), Value::Half(b)) => a $op b,
            (Value::Single(a), Value::Single(b)) => a $op b,
            (Value::Float(a), Value::Float(b)) => a $op b,

            // non number types
            (Value::Tuple(a), Value::Tuple(b)) => a $op b,
            (Value::String(a), Value::String(b)) => a $op b,
            (Value::Bool(x), Value::Bool(y)) => x $op y,

            // stuff you CANNOT do
            (Value::Null, _)                                                => panic!(">//< can't compare to Null!"),
            (_, Value::Null)                                                => panic!(">//< can't compare with Null!"),

            (Value::Void, _)                                                => panic!(">//< can't compare to Void!"),
            (_, Value::Void)                                                => panic!(">//< can't compare with Void!"),

            _ => panic!(">//< can't compare these types!"),
        }
    }};
}

impl Eq for Value {}
impl PartialEq for Value {
    fn eq(&self, other: &Self) -> bool { typical_cmp!(self, other, ==) }
    fn ne(&self, other: &Self) -> bool { typical_cmp!(self, other, !=) }
}

impl Add for Value {
    type Output = Self;
    fn add(self, rhs: Self) -> Self::Output {
        return match (&self, &rhs) {
            (Self::String(a), Self::String(b)) => Self::String(a.to_owned() + b.as_str()),
            _ => typical_op!(self, rhs, +)
        }
    }
}

impl Sub for Value {
    type Output = Self;
    fn sub(self, rhs: Self) -> Self::Output {
        return typical_op!(self, rhs, -)
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

///////////////////////////////////////////////////////////////////////////////
// statements
///////////////////////////////////////////////////////////////////////////////

#[derive(Debug, Clone)]
pub enum Statement {
    VariableAssignment {
        left: String,  // variable name
        right: Box<Expr>,
    },
    Expression(Expr),

    // control flow
    Return(Expr),
    Break(Expr),
    Continue,

    // if-else
    If {
        condition: Expr,
        on_true: Block,
    },
}

///////////////////////////////////////////////////////////////////////////////
// expressions
///////////////////////////////////////////////////////////////////////////////

#[derive(Debug, Clone)]
pub enum Expr {
    Literal(Value),
    Variable(String),
    FunctionCall {
        name: String,
        args: Vec<Expr>,
    },
    BinaryOp {
        left: Box<Expr>,
        right: Box<Expr>,
        op: String
    },
    Loop(Box<Block>)
}

///////////////////////////////////////////////////////////////////////////////
// blocks
///////////////////////////////////////////////////////////////////////////////

#[derive(Debug, Clone)]
pub struct Block(pub Vec<Statement>);
