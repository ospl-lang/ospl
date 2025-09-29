use crate::{ospl::{
    interpreter::{
        Context,
        Interpreter
    },
    Expr,
    Value
}, Block, Statement};

use std::{cell::RefCell, fs::File, io::Read};
use std::rc::Rc;

pub mod spec;
pub mod literal;
pub mod statement;
pub mod controlflow;
pub mod preprocess;

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
            .unwrap_or('\0');  // string terminator
            // .unwrap_or_else(|| self.parse_error("unexpected EOF in peek_or_consume"));

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

    #[allow(dead_code)]  // turn dead code off here, as I know it'll be used later
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
        loop {
            self.consume_while(|c| c.is_whitespace());

            // endl comments
            if self.peek_or_consume('#') {
                self.consume_while(|c| c != '\n');
            }

            // multiline comments
            else if self.match_next("*****") {
                while !self.match_next("*****") {
                    self.next_char();
                }
            }

            // nothing left to skip
            else {
                break;
            }
        }
    }
}

impl Parser {
    /// parses into a `Block` if it finds one
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
                    // try to do preprocessor stuff (this handles backtracking for us)
                    if self.process_preprocessor_directive(&mut stmts) {
                        continue;
                    }

                    let stmt = self.stmt()?;  // parses the statement
                    stmts.push(stmt);
    
                    // now handle the separator
                    self.skip_ws();
                },
                _ => self.parse_error("unexpected EOF in stmts")
            }
        }

        self.skip_ws();
        return Some(stmts)
    }
    
    fn module_root_stmts(&mut self) -> Option<Vec<Statement>> {
        let mut stmts: Vec<Statement> = Vec::new();
        loop {
            self.skip_ws();
            match self.peek() {
                Some(';') => {
                    self.pos += 1;
                    continue;
                }
                Some(_) => {
                    // try to do preprocessor stuff (this handles backtracking for us)
                    if self.process_preprocessor_directive(&mut stmts) {
                        continue;
                    }

                    // now try a statement
                    if let Some(stmt) = self.stmt() {
                        stmts.push(stmt);
                        self.skip_ws();
                    } else {
                        break;
                    }
                },

                // on EOF we just return
                _ => break
            }
        }

        self.skip_ws();
        return Some(stmts)
    }

    const RESERVED_WORDS: &[&str] = &[
        // reserved words
        "loop", "obj", "mix", "cls", "return", "if", "else", "select",
        "check", "case", "destruct", "from", "print", "new", "fn",
        
        // types
        "byte", "BYTE", "word", "WORD", "dword", "DWORD", "qword", "QWORD",
        "half", "single", "float", "str", "ref",
    ];

    /// parse a single identifier
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

        // ensure it contains nothing reserved

        if Self::RESERVED_WORDS.contains(&id.as_str()) {
            // this IS NOT A PARSE ERROR!
            return None
        }

        return Some(id);
    }

    const GROUP_OPEN: &str = "[";
    const GROUP_CLOSE: &str = "]";
    pub fn prefix_expr(&mut self) -> Option<Expr> {
        // ==== DO THE LHS ====
        let lhs = if self.match_next(Self::GROUP_OPEN) {
            // ==== GROUPING ====
            self.skip_ws();
            let inner = self.expr()?;                       // parse inside group
            self.skip_ws();

            if self.match_next(Self::GROUP_CLOSE) { inner }
            else { self.parse_error("invalid grouping expression (expected group closing marker)") }
        }
        
        else if let Some(v) = self.attempt(Self::literal) {
            // ==== LITERALS ====
            v
        }
        
        else if self.match_next("use ") {  // use statement
            self.skip_ws();

            // ugly but who the fuck cares
            let path = self.raw_string_literal()
                .expect("expected (raw) string literal for file path")
                .into_id();

            let mut f = File::open(path)
                .expect("failed to open file");

            let mut buff = String::new();
            f.read_to_string(&mut buff)
                .expect("failed to read file");

            // parse the file
            let mut new_parser = Self::new();
            new_parser.feed(&buff);

            let module_root = new_parser.module_root_stmts()
                .expect("failed to parse module root");

            return Some(
                Expr::Import(
                    module_root
                )
            );
        }

        else if let Some(id) = self.attempt(Self::identifier) {
            // ==== VARS ====
            Expr::Variable(
                Box::new(
                    Expr::Literal(
                        Value::String(id)
                    )
                )
            )
        }
        
        else if self.match_next("new ") {  // class construction
            self.skip_ws();
            let class = self.expr()
                .unwrap_or_else(|| self.parse_error("invalid class after `new`"));

            Expr::Construct(
                Box::new(
                    class
                )
            )
        }
        
        else if self.peek_or_consume('@') {  // deref
            // whitespace is not allowed
            let expr = self.expr()
                .unwrap_or_else(|| self.parse_error("expected expression to deref"));

            Expr::Deref(
                Box::new(expr)
            )
        }
        
        else if self.peek_or_consume('$') {  // ref
            let expr = self.expr()
                .unwrap_or_else(|| self.parse_error("expected expression to ref"));

            Expr::Ref(
                Box::new(expr)
            )
        }
        
        else {
            // WE HAVE NO IDEA WHAT THE LHS IS
            return None;
        };

        return Some(lhs)
    }

    const PROP_ACCESS_CHAR: char = '.';
    const PROP_DYN_ACCESS_CHAR: char = ':';
    pub fn expr(&mut self) -> Option<Expr> {
        let mut lhs = self.prefix_expr()?;

        // ==== POSTFIX OPS ====
        loop {
            self.skip_ws();

            // ==== ALL THE CODE THAT FOLLOWS WAS WRITTEN BY AN LLM ===
            // ...thanks Kevin, and Colton...
            // the comments were mostly written by humans though.

            // property access
            if self.peek_or_consume(Self::PROP_ACCESS_CHAR) {
                self.skip_ws();
                if let Some(ident) = self.identifier() {
                    lhs = Expr::Property(
                        Box::new(lhs),
                        Box::new(Expr::Literal(Value::String(ident))),
                    );
                    continue;
                } else {
                    self.parse_error("Expected identifier after property access");
                }
            }

            // property access (but dynamic this time)
            if self.peek_or_consume(Self::PROP_DYN_ACCESS_CHAR) {
                self.skip_ws();
                if let Some(ident) = self.expr() {  // surely that won't blow the stack...
                    lhs = Expr::Property(
                        Box::new(lhs),
                        Box::new(ident),
                    );
                    continue;
                } else {
                    self.parse_error("Expected expr after dynamic property access");
                }
            }

            // function call
            if self.peek_or_consume('(') {
                let mut fnargs = Vec::new();
                loop {
                    self.skip_ws();
                    match self.peek() {
                        Some(')') => { self.pos += 1; break; }
                        Some(',') => { self.pos += 1; continue; }
                        Some(_) => {
                            if let Some(arg) = self.expr() {
                                fnargs.push(arg);
                            } else { break; }
                        }
                        _ => break
                    }
                }
                lhs = Expr::FunctionCall {
                    left: Box::new(lhs),
                    args: fnargs,
                };
                continue;
            }

            break;
        }

        // ==== INFIX OPS ====
        self.skip_ws();
        if let Some(op) = self.find_peek_or_consume(vec![
            "+", "-", "*", "/", "%", ">=", "<=", "==", "!=", "<", ">",
            "|", "||", "&", "&&"]) {
            self.skip_ws();
            if let Some(rhs) = self.expr() {
                lhs = Expr::BinaryOp {
                    left: Box::new(lhs),
                    right: Box::new(rhs),
                    op,
                };
            }
        }

        Some(lhs)
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
        else if let Some(s) = self.attempt(Self::break_statement) {  // break without a value
            return Some(s)
        }

        // ==== CONTINUE ====
        else if let Some(s) = self.attempt(Self::continue_statement) {
            return Some(s)
        }

        // ==== IF/ELSE ====
        else if let Some(s) = self.attempt(Self::if_statement) {
            return Some(s)
        }

        // ==== CHECK ====
        else if let Some(s) = self.attempt(Self::parse_check) {
            return Some(s)
        }

        // ==== SELECT ====
        else if let Some(s) = self.attempt(Self::parse_select) {
            return Some(s)
        }

        // ==== LOOPS ====
        else if let Some(s) = self.attempt(Self::parse_loop) {
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

    // DONT LEAVE THIS IN PROD DUMBFUCK
    // println!("ast: {:#?}", &ast);

    let _ = Interpreter::block(ctx.clone(), ast);
    /* println!("{:#?}", result);
    println!("{:#?}", ctx); */
}

pub fn stmts(ctx: Rc<RefCell<Context>>, p: &mut Parser, s: &str) {
    p.feed(s);
    p.skip_ws();  // go to the first meaningful item
    let ast = p.module_root_stmts().expect("invalid or no AST.");

    // DONT LEAVE THIS IN PROD DUMBFUCK
    // println!("ast: {:#?}", &ast);

    let _ = Interpreter::block(ctx.clone(), Block(ast));
    /* println!("{:#?}", result);
    println!("{:#?}", ctx); */
}
