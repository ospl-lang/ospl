use crate::{ospl::{
    interpreter::{
        Context,
        Interpreter
    },
    Expr,
    Value
}, Statement};

use log::trace;
use std::cell::RefCell;
use std::rc::Rc;

pub struct Parser {
    input: String, // owned buffer
    pos: usize,    // current cursor
}

impl Parser {
    pub fn new() -> Self {
        Self {
            input: String::new(),
            pos: 0,
        }
    }

    pub fn feed(&mut self, s: &str) {
        self.input = s.to_string(); // replace buffer
        self.pos = 0;
    }
}

impl Parser {
    fn peek(&self) -> Option<char> {
        return self.input[self.pos..].chars().next();
    }

    fn next_char(&mut self) -> Option<char> {
        let c = self.peek()?;
        self.pos += c.len_utf8();
        return Some(c);
    }

    fn peek_or_consume(&mut self, target: char) -> bool {
        let c = self.peek()
            .unwrap_or_else(|| self.parse_error("unexpected EOF"));

        if c == target {
            self.pos += c.len_utf8();
            return true
        } else {
            return false
        }
    }

    fn consume_while<F>(&mut self, mut f: F) -> String
    where
        F: FnMut(char) -> bool,
    {
        let start = self.pos;
        while let Some(c) = self.peek() {
            if f(c) {
                self.next_char();
            } else {
                break;
            }
        }
        return self.input[start..self.pos].to_string();
    }

    fn attempt<F, R>(&mut self, f: F) -> Option<R>
    where
        F: FnOnce(&mut Self) -> Option<R>,
    {
        let snapshot = self.pos;         // save cursor
        if let Some(result) = f(self) { // try parser branch
            Some(result)                // success, keep cursor advanced
        } else {
            self.pos = snapshot;        // fail, rewind
            None
        }
    }

    fn parse_error(&mut self, msg: &str) -> ! {
        panic!(">//< syntax or parse error at character {}: {}", self.pos, msg)
    }

    fn expect_char(&mut self, target: char) -> Option<char> {
        let next = self.next_char()
                            .unwrap_or_else(|| self.parse_error(&format!(
                                            "unexpected EOF, expected `{}`",
                                            target)));

        self.pos += next.len_utf8();

        if next == target {
            return Some(next)
        } else {
            return None
        }
    }

    fn match_next(&mut self, thing: &str) -> bool {
        if self.input[self.pos..].starts_with(thing) {
            self.pos += thing.len();
            return true
        } else {
            return false
        }
    }

    fn find(&mut self, things: Vec<&str>) -> Option<String> {
        for thing in things {
            if self.input[self.pos..].starts_with(thing) {
                self.pos += thing.len();  // <- consume the operator!
                return Some(thing.to_string());
            }
        }
        None
    }

    fn find_peek(&mut self, things: Vec<&str>) -> Option<String> {
        for thing in things {
            if self.input[self.pos..].starts_with(thing) {
                // no consuming
                return Some(thing.to_string());
            }
        }
        return None;
    }

    fn find_peek_or_consume(&mut self, things: Vec<&str>) -> Option<String> {
        if let Some(x) = self.find_peek(things) {
            self.pos += x.len();
            return Some(x)
        } else {
            return None
        }
    }

    pub fn skip_ws(&mut self) {
        self.consume_while(|c| c.is_whitespace());
    }
}

// === LITERALS ===
impl Parser {
    fn number_literal_whole(&mut self) -> Option<Value> {
        // consume the number itself
        let numstr = self.consume_while(|c| c.is_ascii_digit());
        if numstr.is_empty() { return None; }

        // following it should be the postfix
        if let Some(postfix) = self.next_char() {
            return match postfix {
                // temporary but might just become official syntax
                'b' => Some(Value::Byte(numstr.parse::<u8>().expect("expected valid u8"))),
                'B' => Some(Value::SignedByte(numstr.parse::<i8>().expect("expected valid i8"))),

                'w' => Some(Value::Word(numstr.parse::<u16>().expect("expected valid u16"))),
                'W' => Some(Value::SignedWord(numstr.parse::<i16>().expect("expected valid i16"))),

                'd' => Some(Value::DoubleWord(numstr.parse::<u32>().expect("expected valid u32"))),
                'D' => Some(Value::SignedDoubleWord(numstr.parse::<i32>().expect("expected valid i32"))),

                'q' => Some(Value::QuadrupleWord(numstr.parse::<u64>().expect("expected valid u64"))),
                'Q' => Some(Value::SignedQuadrupleWord(numstr.parse::<i64>().expect("expected valid i64"))),

                _ => None
            }
        } else {
            // TODO: handle this correctly
            unimplemented!("non-postfix number not implemented")
        }
    }

    fn identifier(&mut self) -> Option<String> {
        let mut id: String = String::new();

        // first char cannot have numbers, hence this special case here.
        let first_char = self.next_char()?;
        if first_char.is_alphabetic() || first_char == '_' {
            id.push(first_char);
        } else {
            return None;
        }

        // NOW it can have numbers
        id.push_str(&self.consume_while(|c| c.is_alphanumeric() || c == '_'));

        return Some(id);
    }

    fn literal(&mut self) -> Option<Value> {
        if let Some(num) = self.attempt(Self::number_literal_whole) {
            return Some(num);
        }

        else { None }
    }

    fn assignment(&mut self) -> Option<Statement> {
        let id: String = self.identifier()?;
        self.skip_ws();
        self.expect_char(Self::ASSIGN_CHAR)?;
        self.skip_ws();
        let rhs: Expr = self.expr()?;

        return Some(Statement::Assign {
            left: Box::new(
                Expr::Literal(
                    Value::String(id)
                )
            ),
            right: Box::new(rhs)
        });
    }

    fn declaration(&mut self) -> Option<Statement> {
        // all declarations start with def, if it doesn't, this clearly isn't
        // a declaration.
        if !self.match_next("def") { return None };
        self.skip_ws();

        // they're followed by a name
        let id: String = self.identifier()?;
        self.skip_ws();

        // and an optional assignment
        let mut initializer: Option<Expr> = None;
        if self.peek_or_consume('=') {
            self.skip_ws();
            initializer = Some(self.expr()?);
            self.skip_ws();
        }

        // construct a statement
        return Some(Statement::VarDeclaration {
            left: Box::new(
                Expr::Literal(
                    Value::String(id)
                )
            ),
            right: Box::new(
                initializer
                .unwrap_or_else(||  // avoid unneeded work
                    Expr::Literal(
                        Value::Null
                    )
                )
            )
        })
    }
}

impl Parser {
    const GROUP_CHAR_START: char = '|';
    const GROUP_CHAR_END: char = '\\';
    const ASSIGN_CHAR: char = '=';

    /// THE THE FUNCTION
    pub fn expr(&mut self) -> Option<Expr> {
        trace!("parse LHS");
        // ==== DO THE LHS ====
        let mut lhs = if self.peek_or_consume(Self::GROUP_CHAR_START) {
            // ==== GROUPING ====
            trace!("group");
            let inner = self.expr()?;                       // parse inside group
            self.expect_char(Self::GROUP_CHAR_END)?;

            inner
        } else if let Some(v) = self.attempt(Self::literal) {
            // ==== LITERALS ====
            trace!("literal");

            Expr::Literal(v)
        } else if let Some(id) = self.attempt(Self::identifier) {
            // ==== VARS ====
            trace!("variable");

            Expr::Variable(Box::new(Expr::Literal(Value::String(id))))
        } else {
            // WE HAVE NO IDEA WHAT THE LHS IS
            trace!("I DONT KNOW WHAT THE LHS IS");

            return None;
        };

        // ==== BINARY OPS ====
        self.skip_ws();
        while let Some(op) = self.find_peek_or_consume(vec!["+", "-", "*", "/", ">=", "<=", "==", "!=", "<", ">"]) {
            trace!("binaryop: {}", op);
            // parse RHS
            self.skip_ws();
            if let Some(rhs) = self.expr() {
                lhs = Expr::BinaryOp {
                    left: Box::new(lhs),
                    right: Box::new(rhs),
                    op,
                };
            } else {
                trace!("failed to parse RHS after operator");
                break;  // failed to parse RHS after operator
            }
        }

        return Some(lhs);
    }

    pub fn stmt(&mut self) -> Option<Statement> {
        // ==== ASSIGNMENT ====
        if let Some(v) = self.attempt(Self::assignment) {
            return Some(v);
        }

        // ==== DECLARATION ====
        else if let Some(v) = self.attempt(Self::declaration) {
            return Some(v)
        };

        // we don't know
        return None
    }
}

fn exe(ctx: Rc<RefCell<Context>>, p: &mut Parser, s: &str) {
    p.feed(s);
    let ast = p.stmt().expect("bad AST");

    //let result = Interpreter::expr(ctx.clone(), ast);
    let result = Interpreter::stmt(ctx.clone(), ast);
    println!("{:#?}", result);
    println!("{:#?}", ctx);
}

pub fn main() {
    env_logger::init();

    // initialize
    let ctx: Rc<RefCell<Context>> = Rc::new(RefCell::new(Context::new(None)));

    // parse
    let mut p: Parser = Parser::new();
    exe(ctx.clone(), &mut p, "def x = 9d");
    exe(ctx.clone(), &mut p, "def y = 10d");
    exe(ctx.clone(), &mut p, "def z = x + y");
}
