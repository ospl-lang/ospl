use crate::{ospl::{
    interpreter::{
        Context,
        Interpreter
    },
    Expr,
    Value
}, Block, Statement};

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
        self.pos = 0usize;
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
            .unwrap_or_else(|| self.parse_error("unexpected EOF in peek_or_consume"));

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
        let snapshot = self.pos;        // save cursor
        if let Some(result) = f(self) {     // try parser branch
            Some(result)                       // success, keep cursor advanced
        } else {
            self.pos = snapshot;               // fail, rewind
            None
        }
    }

    fn parse_error(&mut self, msg: &str) -> ! {
        let context_max = std::cmp::min(self.pos + 26, self.input.len());
        panic!(
            "
syntax or parse error at char {} ({}): {}

{}",
            self.pos,
            &self.input.get(self.pos..self.pos+1).unwrap_or("<unknown>"),
            msg,
            &self.input[self.pos..context_max].trim(),
        )
    }

    fn expect_char(&mut self, target: char) -> Option<char> {
        let next = self.next_char()?;

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

    fn skip_ws(&mut self) {
        self.consume_while(|c|
            c.is_whitespace());
    }
}

pub mod spec;
pub mod literal;
pub mod statement;

impl Parser {
    fn block(&mut self) -> Option<Block> {
        self.skip_ws();

        let stmts = self.stmts()?;

        return Some(Block(stmts));
    }

    fn stmts(&mut self) -> Option<Vec<Statement>> {
        self.expect_char('{')?;
        let mut stmts: Vec<Statement> = Vec::new();
        loop {
            self.skip_ws();
            match self.peek() {
                Some('}') => {
                    self.pos += 1;
                    break;
                },
                Some(';') => {
                    self.pos += 1;
                    continue;
                }
                Some(_) => {
                    let stmt = self.stmt()?;  // parses the statement
                    stmts.push(stmt);
    
                    // now handle the separator
                    self.skip_ws();
                },
                _ => self.parse_error("unexpected EOF in stmts")
            }
        }

        self.skip_ws();
        return Some(stmts);
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

    fn parse_loop(&mut self) -> Option<Expr> {
        if !self.match_next("loop") {
            return None
        }

        self.skip_ws();
        let ret: Block = self.block()
            .unwrap_or_else(|| self.parse_error("a loop requires a body"));
        return Some(
            Expr::Loop(
                Box::new(ret)
            )
        )
    }

    pub fn primary_expr(&mut self) -> Option<Expr> {
        // ==== DO THE LHS ====
        let lhs = if self.match_next("(*") {
            // ==== GROUPING ====
            self.skip_ws();
            let inner = self.expr()?;                       // parse inside group
            self.skip_ws();

            if self.match_next("*)") { inner }
            else { panic!("bad group") }
        } else if let Some(v) = self.attempt(Self::literal) {
            // ==== LITERALS ====
            v
        } else if let Some(id) = self.attempt(Self::identifier) {
            // ==== VARS ====
            Expr::Variable(Box::new(Expr::Literal(Value::String(id))))
        } else {
            // WE HAVE NO IDEA WHAT THE LHS IS
            return None;
        };

        return Some(lhs)
    }

    /// THE THE FUNCTION
    pub fn expr(&mut self) -> Option<Expr> {
        let mut lhs: Expr = self.primary_expr()?;

        // ==== BINARY OPS ====
        let mut is_binaryop = false;
        self.skip_ws();
        while let Some(op) = self.find_peek_or_consume(vec![
            "+", "-", "*", "/", "%", ">=", "<=", "==", "!=", "<", ">",
            "|", "||", "&", "&&"]) {
            
            // parse RHS
            self.skip_ws();
            if let Some(rhs) = self.expr() {
                lhs = Expr::BinaryOp {
                    left: Box::new(lhs),
                    right: Box::new(rhs),
                    op,
                };
                is_binaryop = true;
            } else {
                break;  // failed to parse RHS after operator
            }
        }

        if is_binaryop {
            return Some(lhs);
        }

        // ==== FUNCTION CALLS ====
        // Check for function calls with parentheses
        self.skip_ws();
        if self.peek_or_consume('(') {
            let mut fnargs: Vec<Expr> = Vec::new();
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
                    },
                    Some(_) => {
                        if let Some(arg) = self.expr() {
                            fnargs.push(arg);
                        } else {
                            break;
                        }
                    },
                    _ => break
                }
            }
            return Some(
                Expr::FunctionCall {
                    left: Box::new(lhs),
                    args: fnargs
                }
            );
        }

        // ==== LOOPS ====
        if let Some(s) = self.attempt(Self::parse_loop) {
            return Some(s)
        }

        return Some(lhs);
    }

    pub fn stmt(&mut self) -> Option<Statement> {
        self.skip_ws();
        
        // ==== ASSIGNMENT ====
        if let Some(v) = self.attempt(Self::assignment) {
            return Some(v);
        }

        // ==== DECLARATION ====
        else if let Some(v) = self.attempt(Self::declaration) {
            return Some(v);
        }

        // ==== PRINT =====
        else if let Some(v) = self.attempt(Self::print) {
            return Some(v)
        }

        // ==== RETURN ====
        else if let Some(s) = self.attempt(Self::return_statement) {
            return Some(s)
        }

        // ==== BREAK ====
        else if let Some(s) = self.attempt(Self::break_statement) {
            return Some(s)
        }

        // ==== CONTINUE ====
        else if let Some(s) = self.attempt(Self::continue_statement) {
            return Some(s)
        }

        // a last ditch effort, try a bare expression
        else if let Some(s) = self.attempt(Self::expr) {
            return Some(
                Statement::Expression(s)
            )
        }

        // we don't know
        return None;
    }
}

pub fn stmt(ctx: Rc<RefCell<Context>>, p: &mut Parser, s: &str) {
    p.feed(s);
    let ast = p.stmt().expect("bad AST");

    let result = Interpreter::stmt(ctx.clone(), ast);
    println!("{:#?}", result);
    println!("{:#?}", ctx);
}

pub fn expr(ctx: Rc<RefCell<Context>>, p: &mut Parser, s: &str) {
    p.feed(s);
    let ast = p.expr().expect("bad AST");

    let result = Interpreter::expr(ctx.clone(), ast);
    println!("{:#?}", result);
    println!("{:#?}", ctx);
}

pub fn block(ctx: Rc<RefCell<Context>>, p: &mut Parser, s: &str) {
    p.feed(s);
    p.skip_ws();  // go to the first meaningful item
    let ast = p.block().expect("invalid or no AST.");

    let _ = Interpreter::block(ctx.clone(), ast);
    /* println!("{:#?}", result);
    println!("{:#?}", ctx); */
}
