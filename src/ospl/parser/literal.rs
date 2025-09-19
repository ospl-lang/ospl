use super::Parser;
use crate::{
    Subspec,
    Value,
    Block,
    Expr
};
use std::{
    cell::RefCell, collections::HashMap, rc::Rc
};

impl Parser {
    fn number_literal_whole_bad(&mut self, numstr: &str) -> Option<Value> {
            // try to just parse as a `SignedDoubleWord`
            if let Ok(attempt) = numstr.parse::<i32>() {
                return Some(Value::SignedDoubleWord(attempt))
            } else {
                // this literally CANNOT be a number, at least it's not a valid
                // i32. DO NOT BE FOOLED, THIS IS IN FACT REACHABLE.
                // If you trigger it, you're REALLY STUPID and tried to write
                // a number bigger than 2^31
                return None
            }
    }

    pub fn number_literal_whole(&mut self) -> Option<Value> {
        // consume the number itself
        let numstr = self.consume_while(|c| c.is_ascii_digit());
        if numstr.is_empty() { return None; }

        // following it should be the postfix
        // spaces are NOT allowed between the postfix and digits!
        if let Some(postfix) = self.peek() {
            return match postfix {
                // ykw what this is just official now whatever
                'b' => { self.pos += 'b'.len_utf8(); Some(Value::Byte(
                    numstr.parse::<u8>()
                    .unwrap_or_else(|_| self.parse_error("invalid value for type byte")))) },
                'B' => { self.pos += 'B'.len_utf8(); Some(Value::SignedByte(numstr.parse::<i8>().expect("expected valid i8"))) },

                'w' => { self.pos += 'w'.len_utf8(); Some(Value::Word(numstr.parse::<u16>().expect("expected valid u16"))) },
                'W' => { self.pos += 'W'.len_utf8(); Some(Value::SignedWord(numstr.parse::<i16>().expect("expected valid i16"))) },

                'd' => { self.pos += 'd'.len_utf8(); Some(Value::DoubleWord(numstr.parse::<u32>().expect("expected valid u32"))) },
                'D' => { self.pos += 'D'.len_utf8(); Some(Value::SignedDoubleWord(numstr.parse::<i32>().expect("expected valid i32"))) },

                'q' => { self.pos += 'q'.len_utf8(); Some(Value::QuadrupleWord(numstr.parse::<u64>().expect("expected valid u64"))) },
                'Q' => { self.pos += 'Q'.len_utf8(); Some(Value::SignedQuadrupleWord(numstr.parse::<i64>().expect("expected valid i64"))) },

                // any other character is not a postfix: don't consume it.
                _ => self.number_literal_whole_bad(&numstr),
            }
        } else {
            return self.number_literal_whole_bad(&numstr);
        }
    }

    /// parse a float
    pub fn number_literal_fraction(&mut self) -> Option<Value> {
        // consume the whole part
        let numstr: String = self.consume_while(|c| c.is_ascii_digit() || c == '.');
        if numstr.is_empty() { return None; }

        if let Some(postfix) = self.peek() {
            return match postfix {
                // duplicated because no f16 type!
                'h' => { self.pos += 'h'.len_utf8(); Some(Value::Half(numstr.parse().expect("expected valid f32"))) },
                's' => { self.pos += 's'.len_utf8(); Some(Value::Single(numstr.parse().expect("expected valid f32"))) },

                // 64-bit floats
                'f' => { self.pos += 'f'.len_utf8(); Some(Value::Float(numstr.parse().expect("expected valid f64"))) },
                // not a postfix: don't consume it, fall through to default 64-bit float
                _ => {
                    if let Ok(attempt) = numstr.parse::<f64>() { Some(Value::Float(attempt)) } else { None }
                }
            }
        } else {
            // if we don't see a postfix, assume we want a 64-bit float
            if let Ok(attempt) = numstr.parse::<f64>() {
                return Some(Value::Float(attempt));
            } else {
                unreachable!(
                    "you wrote a float that doesn't parse as a float ({}). {}",
                    numstr,
                    "TELL ME YOUR SECRETS MAGIC MAN",
                )
            }
        }
    }

    pub fn function_literal(&mut self) -> Option<Value> {
        if !self.match_next("fn") {
            return None
        }

        self.skip_ws();
        let spec: Vec<Subspec> = self.spec_def()?;
        self.skip_ws();
        let block: Block = self.block()?;

        return Some(
            Value::Function {
                spec: spec,
                body: block
            }
        )
    }

    pub fn raw_string_literal(&mut self) -> Option<Value> {
        // opening qoute (double qoute)
        self.expect_char('"')?;

        let s: String = self.consume_while(|c| c != '"');

        // closing qoute (single qoute)
        self.expect_char('"')?;
        return Some(
            Value::String(
                s
            )
        )
    }

    pub fn escape_character(&mut self) -> char {
        match self.next_char() {
            Some('\\') => return '\\',  // backslash
            Some('\'') => return '\'',  // single qoute
            Some('\"') => return '\"',  // double qoute
            Some('n') => return '\n',   // newline
            Some('t') => return '\t',   // tab
            _ => self.parse_error("unknown escape character")
        }
    }
    
    pub fn escaped_string_literal(&mut self) -> Option<Value> {
        // opening qoute (single qoute)
        self.expect_char('\'')?;

        let mut s: String = String::new();
        loop {
            match self.next_char() {
                Some('\'') => break,
                Some('\\') => s.push(self.escape_character()),
                Some(ch) => s.push(ch),
                _ => self.parse_error("unexpected EOF in escapred string literal")
            };
        }

        return Some(
            Value::String(
                s
            )
        )
    }

    pub fn tuple_literal(&mut self) -> Option<Expr> {
        self.expect_char('(')?;

        let mut elements: Vec<Rc<RefCell<Expr>>> = Vec::new();
        loop {
            self.skip_ws();
            match self.peek() {
                Some(')') => {
                    self.pos += 1;
                    break;
                },
                Some(',') => {
                    self.pos += 1;
                    continue;
                }
                Some(_) => {
                    let element = Rc::new(
                        RefCell::new(
                            self.expr()?
                        )
                    );  // parses the statement
                    elements.push(element);
    
                    // now handle the separator
                    self.skip_ws();
                },
                _ => self.parse_error("unexpected EOF in tuple literal")
            }
        }
        self.skip_ws();
        return Some(
            Expr::TupleLiteral(elements)
        );
    }

    pub fn parse_kv_pair(&mut self) -> Option<(String, Rc<RefCell<Expr>>)> {
        let id = self.identifier()?;
        self.skip_ws();
        self.expect_char(':')?;
        self.skip_ws();
        let value = self.expr()?;

        return Some((
            id,
            Rc::new(
                RefCell::new(
                    value
                )
            )
        ))
    }

    pub fn mixmap_literal(&mut self) -> Option<Expr> {
        self.expect_char('[')?;
        let mut positionals: Vec<Rc<RefCell<Expr>>> = Vec::new();
        let mut keyed: HashMap<String, Rc<RefCell<Expr>>> = HashMap::new();
        loop {
            self.skip_ws();
            match self.peek() {
                Some(']') => {
                    self.pos += 1;
                    break;
                },
                Some(',') => {
                    self.pos += 1;
                    continue;
                }
                Some(_) => {
                    if let Some((id, ex)) = self.attempt(Self::parse_kv_pair) {
                        keyed.insert(id, ex);
                    } else if let Some(item) = self.attempt(Self::expr) {
                        positionals.push(
                            Rc::new(
                                RefCell::new(
                                    item
                                )
                            )
                        );
                    } else {
                        self.parse_error("invalid mixmap item")
                    }
                }
                _ => self.parse_error("unexpected EOF in mixmap literal")
            }
        }

        return Some(
            Expr::MixmapLiteral { positional: positionals, keyed }
        )
    }

    pub fn literal(&mut self) -> Option<Expr> {
        if let Some(num) = self.attempt(Self::number_literal_whole) {
            return Some(
                Expr::Literal(num)
            );
        }

        else if let Some(num) = self.attempt(Self::number_literal_fraction) {
            return Some(
                Expr::Literal(num)
            );
        }

        else if let Some(v) = self.find_peek_or_consume(vec!["void", "null", "true", "false"]) {
            return match v.as_ref() {
                "void" => Some(
                    Expr::Literal(Value::Void)
                ),
                "null" => Some(
                    Expr::Literal(Value::Null)
                ),
                "true" => Some(
                    Expr::Literal(Value::Bool(true))
                ),
                "false" => Some(
                    Expr::Literal(Value::Bool(false))
                ),
                _ => None
            };
        }

        else if let Some(func) = self.attempt(Self::function_literal) {
            return Some(Expr::Literal(func));
        }

        else if let Some(tuple) = self.attempt(Self::tuple_literal) {
            return Some(tuple);
        }

        else if let Some(mixmap) = self.attempt(Self::mixmap_literal) {
            return Some(mixmap);
        }

        else if let Some(escstr) = self.attempt(Self::escaped_string_literal) {
            return Some(Expr::Literal(escstr));
        }

        else if let Some(rawstr) = self.attempt(Self::raw_string_literal) {
            return Some(Expr::Literal(rawstr));
        }

        else { return None; }
    }
}