use std::{
    cell::RefCell, cmp::Ordering, collections::HashMap, fmt::Display, ops::{Add, Div, Mul, Sub}, rc::Rc
};

/// a function spec
#[derive(Debug, Clone)]
pub enum Subspec {
    Bind(String),
    BindRef(String),
    Ignore,
    Destruct(Vec<Subspec>),
    Rest(String),
}

pub mod interpreter;
pub mod parser;

///////////////////////////////////////////////////////////////////////////////
// values and types
///////////////////////////////////////////////////////////////////////////////

/// Represents a value in OSPL
#[derive(Debug, Clone)]
pub enum Value {
    // constants
    Const(Box<Value>),

    // stable OSPL types
    Ref(Rc<RefCell<Value>>),
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
    SignedQuadrupleWord(i64),

    // can't actually use f16, as it is unstable.
    Half(f32),
    Single(f32),
    Float(f64),

    Bool(bool),
    String(String),
    Tuple(Vec<Rc<RefCell<Value>>>),
    Mixmap {
        ordered: Vec<Rc<RefCell<Value>>>,
        keyed: HashMap<String, Rc<RefCell<Value>>>
    },
    Function {
        spec: Vec<Subspec>,
        body: Block,
    },
    Object {
        symbols: HashMap<String, Rc<RefCell<Value>>>,
        class: Rc<RefCell<Value>>,
    },
    Class {
        parents: Vec<Rc<RefCell<Value>>>,
        symbols: HashMap<String, Rc<RefCell<Value>>>
    }
}

impl Value {
    pub fn into_values(self) -> Vec<Rc<RefCell<Value>>> {
        match self {
            Self::Tuple(vals_rc) => {
                // borrow the Vec<Rc<RefCell<Value>>> and clone the Rc's (cheap)
                vals_rc.clone()
            }

            Self::Mixmap { ordered, .. } => {
                // borrow the Vec<Rc<RefCell<Value>>> and clone the Rc's (cheap)
                ordered.clone()
            }

            _ => vec![Rc::new(RefCell::new(self))],
        }
    }

    pub fn into_id(&self) -> String {
        return match self {
            Self::String(s) => s.to_string(),
            other @ _ => panic!(">//< expected valid identifier of type str, found: {:?}", other)
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
            Self::SignedQuadrupleWord(_)
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
            Value::SignedQuadrupleWord(n) => *n != 0,

            Value::Half(n) => *n != 0.0,
            Value::Single(n) => *n != 0.0,
            Value::Float(n) => *n != 0.0,

            Value::Bool(b) => *b,

            Value::String(s) => !s.is_empty(),
            Value::Tuple(t) => !t.is_empty(),
            Value::Mixmap { ordered, keyed }
                => !ordered.is_empty()
                || !keyed.is_empty(),

            // may or may not work!
            //Value::Ref(inner) => Self::truthiness(inner),
            _ => true
        }
    }

    fn deep_clone(&self) -> Value {
        return match self {
            // containers that need help
            Value::Tuple(vec) => {
                let new_vec = vec.iter()
                    .map(|v| Rc::new(RefCell::new(v.borrow().deep_clone())))
                    .collect();
                return Value::Tuple(new_vec)
            },
            Value::Object { symbols, class} => {
                let hm = symbols.clone();
                return Value::Object {
                    symbols: hm,
                    class: class.clone()
                }
            },

            // silly types
            Value::Ref(rc) => Value::Ref(Rc::new(RefCell::new(rc.borrow().deep_clone()))),
            
            // stupid types
            Value::String(s) => Value::String(s.clone()),

            Value::Byte(w) => Value::Byte(*w),
            Value::SignedByte(w) => Value::SignedByte(*w),

            Value::Word(w) => Value::Word(*w),
            Value::SignedWord(w) => Value::SignedWord(*w),

            Value::DoubleWord(w) => Value::DoubleWord(*w),
            Value::SignedDoubleWord(w) => Value::SignedDoubleWord(*w),

            Value::QuadrupleWord(w) => Value::QuadrupleWord(*w),
            Value::SignedQuadrupleWord(w) => Value::SignedQuadrupleWord(*w),

            Value::Half(w) => Value::Half(*w),
            Value::Single(w) => Value::Single(*w),
            Value::Float(w) => Value::Float(*w),

            // error
            other @ _ => unimplemented!("Deep clone not implemented for: {:?}", other),
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
            (Value::SignedQuadrupleWord(a), Value::SignedQuadrupleWord(b))  => Value::SignedQuadrupleWord(a $op b),

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
            (Value::Null, _)                                                => panic!(">//< can't compare to Null!"),
            (_, Value::Null)                                                => panic!(">//< can't compare with Null!"),

            (Value::Void, _)                                                => panic!(">//< can't compare to Void!"),
            (_, Value::Void)                                                => panic!(">//< can't compare with Void!"),

            _                                                               => panic!(">//< can't compare {:?} with {:?}", $lhs, $rhs),
        }
    }};
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
            (Self::String(a), Self::String(b)) => Self::String(a.to_owned() + b.as_str()),
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

            _ => "<unprintable>",
        };
        write!(f, "{}", thing_to_write)
    }
}

///////////////////////////////////////////////////////////////////////////////
// statements
///////////////////////////////////////////////////////////////////////////////

#[derive(Debug, Clone)]
pub enum Statement {
    Assign {
        left: Box<Expr>,
        right: Box<Expr>,
    },
    VarDeclaration {
        left: Box<Expr>,
        right: Box<Expr>,
    },

    Expression(Expr),

    // control flow
    Return(Expr),
    Break(Expr),
    Continue,

    Print {
        thing: Box<Expr>
    },

    // if-else
    If {
        condition: Expr,
        on_true: Block,
        on_false: Option<Block>  // you don't need an else
    },
}

///////////////////////////////////////////////////////////////////////////////
// expressions
///////////////////////////////////////////////////////////////////////////////

#[derive(Debug, Clone)]
pub enum Expr {
    Literal(Value),
    Variable(Box<Expr>),
    Property(Box<Expr>, Box<Expr>),
    FunctionCall {
        left: Box<Expr>,
        args: Vec<Expr>,
    },
    BinaryOp {
        left: Box<Expr>,
        right: Box<Expr>,
        op: String
    },
    Loop(Box<Block>),
    Deref(Box<Expr>),
    Ref(Box<Expr>),
    Construct(Box<Expr>),

    // stupid motherfuckers that don't want to follow the rules
    TupleLiteral(Vec<Rc<RefCell<Expr>>>),
    ClassLiteral {
        parents: Vec<Rc<RefCell<Expr>>>,
        symbols: HashMap<String, Rc<RefCell<Expr>>>
    },
    MixmapLiteral {
        positional: Vec<Rc<RefCell<Expr>>>,
        keyed: HashMap<String, Rc<RefCell<Expr>>>
    }
}

impl Expr {
    // convinience functions
    pub fn litstr(s: &str) -> Box<Expr> {
        return Box::new(
            Expr::Literal(
                Value::String(
                    s.to_string()
                )
            )
        )
    }

    pub fn var(s: &str) -> Box<Expr> {
        return Box::new(
            Expr::Variable(
                Expr::litstr(s)
            )
        )
    }

    pub fn s_qword(i: u64) -> Box<Expr> {
        return Box::new(
            Expr::Literal(
                Value::QuadrupleWord(
                    i
                )
            )
        )
    }
}

///////////////////////////////////////////////////////////////////////////////
// blocks
///////////////////////////////////////////////////////////////////////////////

#[derive(Debug, Clone)]
pub struct Block(pub Vec<Statement>);
