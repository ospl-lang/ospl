use crate::{Expr, Value};

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

    fn peek(&self) -> Option<char> {
        return self.input[self.pos..].chars().next();
    }

    fn next_char(&mut self) -> Option<char> {
        let c = self.peek()?;
        self.pos += c.len_utf8();
        return Some(c);
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

    fn consume_until<F>(&mut self, mut f: F) -> String
    where
        F: FnMut(char, &str) -> bool,
    {
        let start = self.pos;
        while let Some(c) = self.peek() {
            if f(c, &self.input[start..]) {
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

    fn parse_error(&mut self, msg: &str) {
        panic!("parse error at character {:?}: {}", self.pos, msg)
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
        None
    }

    pub fn skip_ws(&mut self) {
        self.consume_while(|c| c.is_whitespace());
    }
}

// === LITERALS ===
impl Parser {
    pub fn number_literal_whole(&mut self) -> Option<Value> {
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

    pub fn identifier(&mut self) -> Option<String> {
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

    pub fn literal(&mut self) -> Option<Value> {
        if let Some(num) = self.attempt(Self::number_literal_whole) {
            return Some(num);
        }

        else { None }
    }

}

// === BINARY OPERATIONS ===
impl Parser {
    fn binaryop(&mut self) -> Option<Expr> {
        let mut lhs = self.primary()?;  // parse the first value

        while let Some(op) = self.find(vec!["+", "-", "*", "/", "==", "!="]) {
            let rhs = self.expr()?;  // parse next value
            lhs = Expr::BinaryOp {
                left: Box::new(lhs),
                right: Box::new(rhs),
                op,
            };
        }

        Some(lhs)
    }
}

// === EXPRESSIONS ===
impl Parser {
    pub fn primary(&mut self) -> Option<Expr> {
        if let Some(v) = self.attempt(Self::literal) {
            return Some(Expr::Literal(v))
        }

        else if let Some(v) = self.attempt(Self::identifier) {
            return Some(
                Expr::Variable(
                    Box::new(
                        Expr::Literal(
                            Value::String(v)
                        )
                    )
                )
            )
        }

        else { None }
    }

    pub fn expr(&mut self) -> Option<Expr> {
        if let Some(v) = self.primary() {
            return Some(v)
        }

        else if let Some(v) = self.attempt(Self::binaryop) {
            return Some(v)
        }

        else { None }
    }
}

pub fn main() {
    let mut p = Parser::new();
    p.feed("a+1d-2d");
    println!("{:?}", p.expr());
}
