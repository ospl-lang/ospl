use std::{
    cell::RefCell, collections::HashMap, path::PathBuf, rc::{Rc, Weak}
};

use crate::Context;

/// a function spec
#[derive(Debug, Clone)]
pub enum Subspec {
    Bind(String),
    BindRef(String),

    BindTyped(String, Type),
    BindRefTyped(String, Type),

    // yay
    Destruct(Vec<Subspec>),
    LiteralRequirement(Value),
    Ignore,
    ThisRef(String)
}

pub mod interpreter;
pub mod parser;
pub mod ffi;
pub mod casts;

///////////////////////////////////////////////////////////////////////////////
// values and types
///////////////////////////////////////////////////////////////////////////////

#[derive(Debug, Clone, PartialEq)]
pub enum Type {
    Null, Void,
    Byte, SignedByte, Word, SignedWord, DoubleWord, SignedDoubleWord,
    QuadrupleWord, SignedQuadrupleWord, Half, Single, Float,
    Bool, String,
    Tuple, Mixmap, Object, Module, Ref(Option<Box<Type>>),
    MacroFn, RealFn,

    // CFFI types
    ForeignFn, ForeignLib, ForeignSymbol
}

impl Type {
    pub fn as_bitwidth(&self) -> usize {
        match self {
            Type::Bool => size_of::<bool>(),
            Type::Byte => size_of::<u8>(),
            Type::SignedByte => size_of::<i8>(),
            Type::Word => size_of::<u16>(),
            Type::SignedWord => size_of::<i16>(),
            Type::DoubleWord => size_of::<u32>(),
            Type::SignedDoubleWord => size_of::<i32>(),
            Type::QuadrupleWord => size_of::<u64>(),
            Type::SignedQuadrupleWord => size_of::<i64>(),
            Type::Half => size_of::<f32>(),  // f16 is unstable
            Type::Single => size_of::<f32>(),
            Type::Float => size_of::<f64>(),
            _ => panic!("cannot get bitwidth of type {:?}", self)
        }
    }
}

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
    MacroFn {
        spec: Vec<Subspec>,
        body: Block,
    },
    RealFn {
        // weak pointer reason:
        // if the context drops, the function is guaranteed (I think?) to also
        // have been dropped.
        ctx: Weak<RefCell<Context>>,
        spec: Vec<Subspec>,
        body: Block,
    },
    Object {
        symbols: HashMap<String, Rc<RefCell<Value>>>,
    },
    Module {
        // THIS IS REALLY RETARDED BUT I DON'T GIVE A FUCK
        context: Rc<RefCell<Context>>
    },
    ForeignFn {
        library: String,
        symbol: String,
        arg_types: Vec<String>,
        return_type: String,
    },
    ForeignLib {
        library: String,
    },
    ForeignSymbol {
        library: String,
        symbol: String,
    },
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

    pub fn as_values(&self) -> Vec<Rc<RefCell<Value>>> {
        match &self {
            Self::Tuple(vals_rc) => {
                // borrow the Vec<Rc<RefCell<Value>>> and clone the Rc's (cheap)
                vals_rc.clone()
            }

            Self::Mixmap { ordered, .. } => {
                // borrow the Vec<Rc<RefCell<Value>>> and clone the Rc's (cheap)
                ordered.clone()
            }

            _ => vec![Rc::new(RefCell::new(self.to_owned()))],
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
            Value::Object { symbols, .. } => {
                let hm = symbols.clone();
                return Value::Object {
                    symbols: hm,
                }
            },
            Value::Mixmap { keyed, ordered } => {
                let keyed = keyed.clone();
                let ordered = ordered.clone();
                return Value::Mixmap {
                    keyed,
                    ordered
                }
            },

            // silly types
            // this one practically just does nothing
            Value::Bool(b) => Value::Bool(*b),
            Value::Ref(rc) => Value::Ref(rc.clone()),
            
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
            Value::ForeignFn { library, symbol, arg_types, return_type } => Value::ForeignFn {
                library: library.clone(),
                symbol: symbol.clone(),
                arg_types: arg_types.clone(),
                return_type: return_type.clone(),
            },
            Value::ForeignLib { library } => Value::ForeignLib {
                library: library.clone(),
            },
            Value::ForeignSymbol { library, symbol } => Value::ForeignSymbol {
                library: library.clone(),
                symbol: symbol.clone(),
            },
            Value::Null => Value::Null,
            Value::Void => Value::Void,

            // error
            other @ _ => unimplemented!("Deep clone not implemented for: {:?}", other),
        }
    }

    pub fn as_type(&self) -> Type {
        match &self {
            Value::Bool(_) => Type::Bool,
            Value::Byte(_) => Type::Byte,
            Value::SignedByte(_) => Type::SignedByte,
            Value::Word(_) => Type::Word,
            Value::SignedWord(_) => Type::SignedWord,
            Value::DoubleWord(_) => Type::DoubleWord,
            Value::SignedDoubleWord(_) => Type::SignedDoubleWord,
            Value::QuadrupleWord(_) => Type::QuadrupleWord,
            Value::SignedQuadrupleWord(_) => Type::SignedQuadrupleWord,
            Value::Half(_) => Type::Half,
            Value::Single(_) => Type::Single,
            Value::Float(_) => Type::Float,
            Value::String(_) => Type::String,
            Value::Void => Type::Void,
            Value::Null => Type::Null,

            Value::Object { .. } => Type::Object,
            Value::Mixmap { .. } => Type::Mixmap,
            Value::Tuple(_) => Type::Tuple,

            Value::RealFn { .. } => Type::RealFn,
            Value::MacroFn { .. } => Type::MacroFn,
            Value::Ref(to) => Type::Ref(
                Some(
                    Box::new(
                        to.borrow().as_type()
                    )
                )
            ),

            Value::Module { .. } => Type::Module,

            _ => Type::Void
        }
    }

    pub fn as_usize(&self) -> usize {
        match &self {
            Value::Bool(_) => panic!("bool aint a number stoopid"),
            Value::Byte(b) => *b as usize,
            Value::SignedByte(b) =>  *b as usize,
            Value::Word(b) => *b as usize,
            Value::SignedWord(b) => *b as usize,
            Value::DoubleWord(b) => *b as usize,
            Value::SignedDoubleWord(b) => *b as usize,
            Value::QuadrupleWord(b) => *b as usize,
            Value::SignedQuadrupleWord(b) => *b as usize,
            _ => panic!("can't get the number for that STUPID")
        }
    }
}

pub mod value_traits;

//.////////////////////////////////////////////////////////////////////////////
// statements
//.////////////////////////////////////////////////////////////////////////////

#[derive(Debug, Clone)]
pub struct SpannedStatement {
    filename: Rc<str>,  // memory space
    line: usize,
    stmt: Statement
}

impl SpannedStatement {
    pub fn new(line: usize, stmt: Statement, file: Rc<str>) -> SpannedStatement {
        return Self {
            line,
            stmt,
            filename: file
        }
    }
}

// TODO: remove all these uneeded Box<Expr>
#[derive(Debug, Clone)]
pub enum Statement {
    Assign {
        left: Box<Expr>,
        right: Box<Expr>,
    },
    AssignOp {
        left: Expr,
        right: Expr,
        op: String
    },
    Delete {
        left: Box<Expr>,
    },
    Declaration {
        left: Box<Expr>,
        right: Box<Expr>,
    },

    Expression(Expr),

    // control flow
    Return(Expr),
    Break,
    Continue,

    Print {
        thing: Box<Expr>
    },

    // ==== CONTROL FLOW ====
    // if-else
    If {
        condition: Expr,
        on_true: Block,
        on_false: Option<Block>  // you don't need an else
    },

    // switch
    Check {
        matching: Box<Expr>,
        cases: Vec<(Vec<Subspec>, Block)>,
    },
    Select {
        matching: Box<Expr>,
        cases: Vec<(Vec<Subspec>, Block)>,
    },
    Loop(Box<Block>),
    ImportLib {
        name: String,
        path: String,
    },

    Memcopy {
        address: Expr,
        value: Expr,
    }
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
    UnaryOp {
        left: Box<Expr>,
        op: String
    },
    Deref(Box<Expr>),
    Ref(Box<Expr>),

    // stupid motherfuckers that don't want to follow the rules
    TupleLiteral(Vec<Rc<RefCell<Expr>>>),
    ObjectLiteral(HashMap<String, Rc<RefCell<Expr>>>),
    MixmapLiteral {
        positional: Vec<Rc<RefCell<Expr>>>,
        keyed: HashMap<String, Rc<RefCell<Expr>>>
    },
    RealFnLiteral {
        spec: Vec<Subspec>,
        body: Block,
    },
    Import {
        ast: Vec<SpannedStatement>,
        filename: PathBuf,
    },

    TypeCast {
        left: Box<Expr>,
        into: Type,
        mode: TypeCastMode
    },

    // cffi only stuff
    ForeignFunctionLiteral {
        library: String,
        symbol: String,
        arg_types: Vec<String>,
        return_type: String,
    },
    CffiLoad {
        path: String,
    },
    CffiFn {
        target: Box<Expr>,
        arg_types: Vec<String>,
        return_type: String,
    },

    DeepCopy(Box<Expr>)
}

#[derive(Debug, Clone)]
pub enum TypeCastMode {
    Convert,
    Reinterpret,
    PointerReinterpret
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

    pub fn into_value(&self) -> Option<Value> {
        if let Expr::Literal(v) = self {
            return Some(v.clone())
        } else {
            return None
        }
    }
}

///////////////////////////////////////////////////////////////////////////////
// blocks
///////////////////////////////////////////////////////////////////////////////

#[derive(Debug, Clone)]
pub struct Block {
    pub stmts: Vec<SpannedStatement>,
}
