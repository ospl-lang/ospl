use crate::{ospl::{
    interpreter::{
        Context,
        Interpreter
    },
    Expr,
    Value
}, Block, Statement, Subspec};

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
        panic!("syntax or parse error at char {} (`{}` near `{}`...): {}",
            self.pos,
            &self.input[self.pos..self.pos+1],
            &self.input[self.pos..self.pos+24].trim(),
            msg
        )
    }

    fn expect_char(&mut self, target: char) -> Option<char> {
        let next = self.next_char()
                            .unwrap_or_else(|| self.parse_error(&format!(
                                            "unexpected EOF, expected `{}`",
                                            target)));

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

    fn number_literal_whole(&mut self) -> Option<Value> {
        // consume the number itself
        let numstr = self.consume_while(|c| c.is_ascii_digit());
        if numstr.is_empty() { return None; }

        // following it should be the postfix
        // spaces are NOT allowed between the postfix and digits!
        if let Some(postfix) = self.next_char() {
            return match postfix {
                // ykw what this is just official now whatever
                'b' => Some(Value::Byte(numstr.parse::<u8>().expect("expected valid u8"))),
                'B' => Some(Value::SignedByte(numstr.parse::<i8>().expect("expected valid i8"))),

                'w' => Some(Value::Word(numstr.parse::<u16>().expect("expected valid u16"))),
                'W' => Some(Value::SignedWord(numstr.parse::<i16>().expect("expected valid i16"))),

                'd' => Some(Value::DoubleWord(numstr.parse::<u32>().expect("expected valid u32"))),
                'D' => Some(Value::SignedDoubleWord(numstr.parse::<i32>().expect("expected valid i32"))),

                'q' => Some(Value::QuadrupleWord(numstr.parse::<u64>().expect("expected valid u64"))),
                'Q' => Some(Value::SignedQuadrupleWord(numstr.parse::<i64>().expect("expected valid i64"))),

                _ => self.number_literal_whole_bad(&numstr),
            }
        } else {
            return self.number_literal_whole_bad(&numstr);
        }
    }

    /// parse a float
    fn number_literal_fraction(&mut self) -> Option<Value> {
        // consume the whole part
        let numstr: String = self.consume_while(|c| c.is_ascii_digit() || c == '.');
        if numstr.is_empty() { return None; }

        if let Some(postfix) = self.next_char() {
            return match postfix {
                // duplicated because no f16 type!
                'h' => Some(Value::Half(numstr.parse().expect("expected valid f32"))),
                's' => Some(Value::Single(numstr.parse().expect("expected valid f32"))),

                // 64-bit floats
                'f' => Some(Value::Float(numstr.parse().expect("expected valid f64"))),
                _ => None
            }
        } else {
            // if we don't see a postfix, assume we want a 64-bit float
            if let Ok(attempt) = numstr.parse::<f64>() {
                return Some(Value::Float(attempt));
            } else {
                // this is technically reachable if you wrote a MASSIVE NUMBER
                // but that number will probably take more digits to write than
                // atoms in the universe, so as far as I'm concerned, this is
                unreachable!(
                    "you wrote a float that doesn't parse as a float ({}). {}",
                    numstr,
                    "TELL ME YOUR SECRETS MAGIC MAN",
                )
            }
        }
    }

    fn subspec(&mut self) -> Option<Subspec> {
        // BASE CASE(s)
        if let Some(id) = self.attempt(Self::identifier) {
            // we do a binding
            return Some(Subspec::Bind(id))
        }

        else if self.peek_or_consume('$') {
            // we bind by reference
            let Some(id) = self.attempt(Self::identifier)
            else { return None };

            return Some(Subspec::BindRef(id));
        }

        // RECURSIVE CASES
        else if self.peek_or_consume('(') {
            // it's a destruction
            return Some(
                Subspec::Destruct(
                    self.spec_def().expect("a")
                )
            )
        }

        else { None }
    }

    fn spec_def(&mut self) -> Option<Vec<Subspec>> {
        self.expect_char('(')?;

        let mut spec: Vec<Subspec> = Vec::new();
        loop {
            self.skip_ws();
            match self.peek() {
                Some(')') => {
                    self.pos += 1;
                    break;
                },
                Some(_) => {
                    let value = self.subspec()?;
                    spec.push(value);
                }
                _ => self.parse_error("unexpected EOF")
            }
        }

        return Some(spec)
    }

    fn function_literal(&mut self) -> Option<Value> {
        // use a space here because it's a keyword
        if !self.match_next("fn ") {
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

    fn block(&mut self) -> Option<Block> {
        self.expect_char('{')?;
        self.skip_ws();

        let stmts = self.stmts()?;

        return Some(Block(stmts));
    }

    fn stmts(&mut self) -> Option<Vec<Statement>> {
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
                _ => self.parse_error("unexpected EOF")
            }
        }
        self.skip_ws();
        Some(stmts)
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

    fn tuple_literal(&mut self) -> Option<Expr> {
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
                _ => self.parse_error("unexpected EOF")
            }
        }
        self.skip_ws();
        return Some(
            Expr::TupleLiteral(elements)
        );
    }

    fn literal(&mut self) -> Option<Expr> {
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
            return Some(Expr::Literal(func))
        }

        else if let Some(tuple) = self.attempt(Self::tuple_literal) {
            return Some(tuple);
        }

        else { return None; }
    }

    fn print(&mut self) -> Option<Statement> {
        self.skip_ws();
        if !self.match_next("print") {
            return None
        }

        self.skip_ws();
        let ex: Expr = self.expr()?;

        return Some(Statement::Print {
            thing: Box::new(ex)
        });
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
        // use a space here because it's a keyword
        if !self.match_next("def ") {
            return None
        };
        self.skip_ws();

        // they're followed by a name
        let id: String = self.identifier()?;
        self.skip_ws();

        // and an optional assignment
        let mut initializer: Option<Expr> = None;
        if self.peek_or_consume(Self::ASSIGN_CHAR) {
            self.skip_ws();
            initializer = Some(self.expr().unwrap());
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
    const GROUP_CHAR_START: &str = "(*";
    const GROUP_CHAR_END: &str = "*)";

    const ASSIGN_CHAR: char = '=';

    pub fn primary_expr(&mut self) -> Option<Expr> {
        // ==== DO THE LHS ====
        let lhs = if self.match_next(Self::GROUP_CHAR_START) {
            // ==== GROUPING ====
            self.skip_ws();
            let inner = self.expr()?;                       // parse inside group
            self.skip_ws();

            if self.match_next(Self::GROUP_CHAR_END) { inner }
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
        // after parsing LHS, there may be a juxtaposition.
        // Meaning this is a function call!
        // only primaries are valid juxtapositions for... reasons...
        let mut is_fncall = false;
        let mut fnargs: Vec<Expr> = Vec::new();
        while let Some(next) = self.literal() {
            fnargs.push(next);
            self.skip_ws();

            is_fncall = true;
        }

        if is_fncall {
            return Some(
                Expr::FunctionCall {
                    left: Box::new(lhs),
                    args: fnargs
                }
            );
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
            return Some(v);
        }

        // ==== PRINT =====
        else if let Some(v) = self.attempt(Self::print) {
            return Some(v)
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

    let result = Interpreter::block(ctx.clone(), ast);
    /* println!("{:#?}", result);
    println!("{:#?}", ctx); */
}

pub fn main() {
    // initialize
    let ctx: Rc<RefCell<Context>> = Rc::new(RefCell::new(Context::new(None)));

    // parse
    let mut p: Parser = Parser::new();
    /* stmt(ctx.clone(), &mut p, "def x = 9d");
    stmt(ctx.clone(), &mut p, "def y = 10d");
    stmt(ctx.clone(), &mut p, "def z = x + y"); */

    block(ctx.clone(), &mut p, "
    {
        def x = 9;
        def y=10;
        def z=x+y;
    }");
}
