use crate::{ospl::{
    interpreter::{
        Context,
        Interpreter
    }, Expr, SpannedStatement, Value
}, Block, SpannedExpr, Statement};

use std::{cell::RefCell, fs::{self, File}, io::Read, path::{Path, PathBuf}};
use std::rc::Rc;

pub mod spec;
pub mod literal;
pub mod statement;
pub mod controlflow;
pub mod preprocess;
pub mod types;

pub struct Parser {
    input: String, // owned buffer
    pos: usize,    // current cursor

    // position info
    lineno: usize,
    colno: usize,
    filepath: Rc<str>,  // current file
}

impl Parser {
    pub fn new<P: AsRef<Path>>(filepath: P) -> Self {
        let canonical_path: PathBuf = fs::canonicalize(filepath).expect("failed to canoicalize path");
        let real_path: &str = &*canonical_path.to_string_lossy();

        return Self {
            input: String::new(),
            pos: 0,
            lineno: 1,
            colno: 1,
            filepath: Rc::from(real_path),
        }
    }

    // TODO: rewrite ENTIRE FUCKING LANG to save like 0.0001s during parsing! A.k.a STOP THIS FUCKING SHIT!
    pub fn path(&self) -> &Path {
        return Path::new(&*self.filepath)
    }

    pub fn cwd(&self) -> &Path {
        return self.path().parent().unwrap_or_else(|| Path::new("."))
    }

    /// Resolves a relative include (e.g., "fileb.ospl") using this parser's directory.
    pub fn resolve_relative_path(&self, rel: &str) -> PathBuf {
        return self.cwd().join(rel)
    }

    /// Spawns a new parser for a relative include.
    pub fn new_rel(&self, rel: &str) -> Self {
        let resolved = self.resolve_relative_path(rel);
        return Self::new(resolved)
    }

    pub fn subparser(&mut self, p: &str) -> Self {
        let resolved: PathBuf = self.resolve_relative_path(&p);

        let mut file: File = File::open(&resolved)
            .expect("failed to include file (failed to open it, does the file exist?)");

        // read our file
        let mut buffer: String = String::new();
        file.read_to_string(&mut buffer)
            .expect("failed to read file");

        let mut parser: Parser = Self::new(&resolved);
        parser.feed(&buffer);
        return parser
    }

    pub fn feed(&mut self, s: &str) {
        self.input = s.to_string(); // replace buffer
        self.pos = 0usize;
        self.lineno = 1usize;
        self.colno = 1usize;
    }
}

impl Parser {
    fn peek(&self) -> Option<char> {
        return self.input[self.pos..].chars().next();
    }

    fn next_char(&mut self) -> Option<char> {
        let c = self.peek()?;
        self.pos += c.len_utf8();

        // newline cheeeeeeeck!
        if c == '\n' {
            self.lineno += 1;
            self.colno = 1;
        } else {
            self.colno += 1;
        }

        return Some(c);
    }

    fn peek_or_consume(&mut self, target: char) -> bool {
        let c = self.peek()
            .unwrap_or('\0');

        if c == target {
            self.next_char();
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
            // no newline check cuz its done for us
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
        let snapshot = self.pos;        // save cursors
        let snapshot_line = self.lineno;
        let snapshot_col = self.colno;
        if let Some(result) = f(self) {     // try parser branch
            Some(result)                       // success, keep cursor advanced
        } else {
            self.pos = snapshot;               // fail, rewind
            self.lineno = snapshot_line;
            self.colno = snapshot_col;
            None
        }
    }

    fn parse_error(&mut self, msg: &str) -> ! {
        let context_max = std::cmp::min(self.pos + 26, self.input.len());
        panic!(
            "
parse error: {}:{}:{} ({} chars in at {})
near:
{}

{}",
            self.filepath,
            self.lineno,
            self.colno,
            self.pos,
            &self.input.get(self.pos..self.pos+1).unwrap_or("<unknown>"),
            &self.input[self.pos..context_max].trim(),
            msg,
        );
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
            self.lineno += thing.chars().filter(|&c| c == '\n').count();  // man I really don't wanna do this...
            self.pos += thing.len();
            return true
        } else {
            return false
        }
    }

    fn find_peek(&mut self, things: Vec<&str>) -> Option<String> {
        for thing in things {
            if self.input[self.pos..].starts_with(thing) {
                // no consuming
                return Some(thing.to_string());
            }
        }
        return None
    }

    fn find_peek_or_consume(&mut self, things: Vec<&str>) -> Option<String> {
        // no newline check cuz the thing calls the other thing and that other thing does the check
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

        return Some(
            Block {
                stmts,
            }
        );
    }

    fn stmts(&mut self) -> Option<Vec<SpannedStatement>> {
        self.expect_char('{')?;
        let mut stmts: Vec<SpannedStatement> = Vec::new();
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
    
    pub fn module_root_stmts(&mut self) -> Option<Vec<SpannedStatement>> {
        let mut stmts: Vec<SpannedStatement> = Vec::new();
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
        "loop", "obj", "mix", "return", "if", "else", "select", "check",
        "case", "destruct", "from", "print", "fn", "foreign", "import",

        // types
        "byte", "BYTE", "word", "WORD", "dword", "DWORD", "qword", "QWORD",
        "half", "single", "float", "str", "ref", "refto", "tuple", "copyof"
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
    pub fn prefix_expr(&mut self) -> Option<SpannedExpr> {
        // ==== DO THE LHS ====
        let lhs: SpannedExpr = 
        
        if self.match_next(Self::GROUP_OPEN) {
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

        else if self.match_next(Self::OSPL_CFFI_LOAD_KW) {
            self.skip_ws();
            let path = self.expr()
                .unwrap_or_else(|| self.parse_error("expected raw string literal for OSPL_CFFI_Load"));

            self.new_spanned_expr(Expr::CffiLoad { path: Box::new(path) })
        }

        else if self.match_next(Self::OSPL_CFFI_FN_KW) {
            self.skip_ws();
            let target_expr = self.parse_cffi_target();

            self.skip_ws();
            self.expect_char('(')
                .unwrap_or_else(|| self.parse_error("expected '(' before argument type list"));

            let mut arg_types = Vec::new();
            loop {
                self.skip_ws();
                if self.peek_or_consume(')') { break; }

                let ty = self.identifier()
                    .unwrap_or_else(|| self.parse_error("expected argument type identifier"));
                arg_types.push(ty);

                self.skip_ws();
                if self.peek_or_consume(',') {
                    continue;
                } else if self.peek_or_consume(')') {
                    break;
                } else {
                    self.parse_error("expected ',' or ')' in argument type list");
                }
            }

            self.skip_ws();
            self.match_next("-> ")
                .then_some(())
                .unwrap_or_else(|| self.parse_error("expected '->' before return type"));

            self.skip_ws();
            let return_type = self.identifier()
                .unwrap_or_else(|| self.parse_error("expected return type identifier"));

            self.new_spanned_expr(
                Expr::CffiFn {
                    target: Box::new(target_expr),
                    arg_types,
                    return_type,
                }
            )
        }
        
        else if self.match_next("use ") {  // use statement
            self.skip_ws();

            // ugly but who the fuck cares
            let path: String = self.raw_string_literal()
                .unwrap_or_else(|| self.parse_error("expected valid raw string literal for file path"))
                .into_id();

            // parse the file
            let mut new_parser: Parser = self.subparser(&path);

            let module_root = new_parser.module_root_stmts()
                .unwrap_or_else(|| self.parse_error("failed to include the file (does it have errors?)"));

            return Some(
                self.new_spanned_expr(
                    Expr::Import {
                        ast: module_root,
                        filename: PathBuf::from(path)
                    }
                )
            );
        }

        // what
        else if self.match_next("foreign ") {
            self.skip_ws();

            let lib = self.raw_string_literal()
                .unwrap_or_else(|| self.parse_error("expected library name"))
                .into_id();

            self.skip_ws();

            let symbol = self.raw_string_literal()
                .unwrap_or_else(|| self.parse_error("expected symbol name"))
                .into_id();

            self.skip_ws();

            self.expect_char('(')
                .unwrap_or_else(|| self.parse_error("expected '('"));

            let mut arg_types = Vec::new();
            loop {
                self.skip_ws();
                if self.peek_or_consume(')') {
                    break;
                }

                let arg = self.identifier()
                    .unwrap_or_else(|| self.parse_error("expected arg type"));
                arg_types.push(arg);

                self.skip_ws();
                if self.peek_or_consume(',') {
                    continue;
                } else if self.peek_or_consume(')') {
                    break;
                } else {
                    self.parse_error("expected ',' or ')'" );
                }
            }

            self.skip_ws();
            self.match_next("-> ")
                .then_some(())
                .unwrap_or_else(|| self.parse_error("expected '->'"));

            self.skip_ws();
            let return_type = self.identifier()
                .unwrap_or_else(|| self.parse_error("expected return type"));

            self.new_spanned_expr(
                Expr::ForeignFunctionLiteral {
                    library: lib,
                    symbol,
                    arg_types,
                    return_type,
                }
            )
        }

        else if let Some(id) = self.attempt(Self::identifier) {
            // ==== VARS ====

            // ugly
            self.new_spanned_expr(
                Expr::Variable(
                    Box::new(
                        self.new_spanned_expr(
                            Expr::Literal(
                                Value::String(id)
                            )
                        )
                    )
                )
            )
        }
        
        // DRY violation speedrun any% - tied WR with that one other piece of code...
        else if self.peek_or_consume('@') {  // deref
            // whitespace is not allowed
            let expr = self.expr()
                .unwrap_or_else(|| self.parse_error("expected expression to deref"));

            self.new_spanned_expr(
                Expr::Deref(
                    Box::new(expr)
                )
            )
        }
        
        else if self.peek_or_consume('$') {  // ref
            let expr = self.expr()
                .unwrap_or_else(|| self.parse_error("expected expression to ref"));

            self.new_spanned_expr(
                Expr::Ref(
                    Box::new(expr)
                )
            )
        }
        
        else {
            // WE HAVE NO IDEA EHSY THE LHS IS
            return None;
        };

        return Some(lhs)
    }

    const OSPL_CFFI_FN_KW: &str = "OSPL_CFFI_Fn ";
    const OSPL_CFFI_LOAD_KW: &str = "OSPL_CFFI_Load ";

    const PROP_ACCESS_CHAR: char = '.';
    const PROP_DYN_ACCESS_CHAR: char = ':';
    pub fn expr(&mut self) -> Option<SpannedExpr> {
        let mut lhs: SpannedExpr = self.prefix_expr()?;

        // ==== POSTFIX OPS ====
        loop {
            self.skip_ws();

            // CFFI load literal
            if self.match_next(Self::OSPL_CFFI_LOAD_KW) {
                self.skip_ws();
                let path = self.expr()
                    .unwrap_or_else(|| self.parse_error("expected path after CFFI_Load"));
                lhs = self.new_spanned_expr(
                    Expr::CffiLoad { path: Box::new(path) }
                );
                continue;
            }

            // CFFI function binding
            if self.match_next(Self::OSPL_CFFI_FN_KW) {
                self.skip_ws();

                let target_expr = self.expr()
                    .unwrap_or_else(|| self.parse_error("expected expression after ?!CFFI_Fn"));

                self.skip_ws();
                self.expect_char('(')
                    .unwrap_or_else(|| self.parse_error("expected '(' before argument type list"));

                let mut arg_types = Vec::new();
                loop {
                    self.skip_ws();
                    if self.peek_or_consume(')') { break; }

                    let ty = self.identifier()
                        .unwrap_or_else(|| self.parse_error("expected argument type identifier"));
                    arg_types.push(ty);

                    self.skip_ws();
                    if self.peek_or_consume(',') {
                        continue;
                    } else if self.peek_or_consume(')') {
                        break;
                    } else {
                        self.parse_error("expected ',' or ')' in argument type list");
                    }
                }

                self.skip_ws();
                self.match_next("-> ")
                    .then_some(())
                    .unwrap_or_else(|| self.parse_error("expected '->' before return type"));

                self.skip_ws();
                let return_type = self.identifier()
                    .unwrap_or_else(|| self.parse_error("expected return type identifier"));

                lhs = self.new_spanned_expr(
                    Expr::CffiFn {
                        target: Box::new(target_expr),
                        arg_types,
                        return_type,
                    }
                );
                continue;
            }

            // property access
            if self.peek_or_consume(Self::PROP_ACCESS_CHAR) {
                self.skip_ws();
                if let Some(ident) = self.identifier() {
                    lhs = self.new_spanned_expr(
                        Expr::Property(
                            Box::new(lhs),
                            Box::new(
                                self.new_spanned_expr(Expr::Literal(Value::String(ident)))
                            ),
                        )
                    );
                    continue;
                } else {
                    self.parse_error("Expected identifier after property access");
                }
            }

            // property access (but dynamic this time)
            if self.peek_or_consume(Self::PROP_DYN_ACCESS_CHAR) {
                self.skip_ws();
                if let Some(ident) = self.prefix_expr() {  // surely that won't blow the stack...
                    lhs = self.new_spanned_expr(
                        Expr::Property(
                            Box::new(lhs),
                            Box::new(ident),
                        )
                    );
                    continue;
                } else {
                    self.parse_error("Expected expr after dynamic property access");
                }
            }

            // casting (type conversion)
            if self.match_next("as ") {
                let ty = self.typedef().unwrap_or_else(|| self.parse_error("expected typedef"));
                return Some(
                    self.new_spanned_expr(
                        Expr::TypeCast {
                            left: Box::new(lhs),
                            into: ty,
                            mode: crate::TypeCastMode::Convert
                        }
                    )
                )
            }

            // casting (pointer reinterpret)
            if self.match_next("asPointerReinterpret ") {
                let ty = self.typedef().unwrap_or_else(|| self.parse_error("expected typedef"));
                return Some(
                    self.new_spanned_expr(
                        Expr::TypeCast {
                            left: Box::new(lhs),
                            into: ty,
                            mode: crate::TypeCastMode::PointerReinterpret
                        }
                    )
                )
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
                lhs = self.new_spanned_expr(
                    Expr::FunctionCall {
                        left: Box::new(lhs),
                        args: fnargs,
                    }
                );
                continue;
            }

            break;
        }

        // ==== INFIX OPS ====
        self.skip_ws();
        if let Some(op) = self.find_peek_or_consume(vec![
            "+", "-", "*", "/", "%", ">=", "<=", "==", "!=", ">>", "<<", "&&", "||",
            "<", ">", "|", "&"
        ]) {
            self.skip_ws();
            if let Some(rhs) = self.expr() {
                lhs = self.new_spanned_expr(
                    Expr::BinaryOp {
                        left: Box::new(lhs),
                        right: Box::new(rhs),
                        op,
                    }
                );
            }
        }

        Some(lhs)
    }


    fn parse_cffi_target(&mut self) -> SpannedExpr {
        self.skip_ws();

        let base_id = self.identifier()
            .unwrap_or_else(|| self.parse_error("expected identifier for CFFI target"));

        // weird recursion kind of thing here... ehsy?
        let mut expr: SpannedExpr = self.new_spanned_expr(
            Expr::Variable(
                Box::new(
                    self.new_spanned_expr(
                        Expr::Literal(
                            Value::String(
                                base_id  // gives up ownership I think?
                            )
                        )
                    )
                )
            )
        );

        loop {
            self.skip_ws();

            if self.peek_or_consume(Self::PROP_ACCESS_CHAR) {
                self.skip_ws();
                let ident = self.identifier()
                    .unwrap_or_else(|| self.parse_error("expected identifier after '.' in CFFI target"));
                expr = self.new_spanned_expr(
                    Expr::Property(
                        Box::new(expr),
                        Box::new(
                            self.new_spanned_expr(
                                Expr::Literal(
                                    Value::String(
                                        ident
                                    )
                                )
                            )
                        )
                    )
                );
                continue;
            }

            if self.peek_or_consume(Self::PROP_DYN_ACCESS_CHAR) {
                self.skip_ws();
                let ident_expr = self.expr()
                    .unwrap_or_else(|| self.parse_error("expected expression after ':' in CFFI target"));
                expr = self.new_spanned_expr(
                    Expr::Property(
                        Box::new(expr),
                        Box::new(ident_expr),
                    )
                );
                continue;
            }

            break;
        }

        return expr
    }

    pub fn stmt(&mut self) -> Option<SpannedStatement> {
        self.skip_ws();

        // ==== ASSIGN OPS ====
        if let Some(assign_op) = self.attempt(Self::assign_op) {
            return Some(assign_op);
        }

        // ==== ASSIGNMENT ====
        else if let Some(v) = self.attempt(Self::assignment) {
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

        // ==== IMPORT LIBRARY ====
        else if let Some(s) = self.attempt(Self::import_lib) {
            return Some(s)
        }

        // ==== BAD IDEA ====
        else if let Some(s) = self.attempt(Self::bad_idea) {
            return Some(s)
        }

        // a last ditch effort, try a bare expression
        else if let Some(s) = self.attempt(Self::expr) {
            return Some(
                SpannedStatement::new(
                    self.lineno,
                    Statement::Expression(s),
                    self.filepath.clone()
                )
            )
        }

        // we don't know
        return None;
    }

    fn assign_op(&mut self) -> Option<SpannedStatement> {
        let left = self.expr()?;
        self.skip_ws();

        let Some(op) = self.find_peek_or_consume(vec!["+=", "-=", "*=", "/=", "||=", "&&=", "^^=", "!!="])
            else { return None };

        self.skip_ws();
        let right = self.expr()
            .unwrap_or_else(|| self.parse_error("expected right-hand side after assign operator"));

        return Some(
            SpannedStatement::new(
                self.lineno,
                Statement::AssignOp {
                    left, right, op
                },
                self.filepath.clone()
            )
        )
    }
}

pub fn stmt(ctx: Rc<RefCell<Context>>, p: &mut Parser, s: &str) {
    p.feed(s);
    let ast = p.stmt().expect("bad AST. (please turn on RUST_BACKTRACE=1 and report the logs to us!)");

    let result = Interpreter::stmt(ctx.clone(), &ast);  // ehsy?
    println!("{:#?}", result);
    println!("{:#?}", ctx);
}

pub fn expr(ctx: Rc<RefCell<Context>>, p: &mut Parser, s: &str) {
    p.feed(s);
    let ast = p.expr().expect("bad AST. (please turn on RUST_BACKTRACE=1 and report the logs to us!)");

    let result = Interpreter::expr(ctx.clone(), &ast);
    println!("{:#?}", result);
    println!("{:#?}", ctx);
}

pub fn block(ctx: Rc<RefCell<Context>>, p: &mut Parser, s: &str) {
    p.feed(s);
    p.skip_ws();  // go to the first meaningful item
    let ast = p.block().expect("invalid or no AST. (please turn on RUST_BACKTRACE=1 and report the logs to us!)");

    // DONT LEAVE THIS IN PROD DUMBFUCK
    // println!("ast: {:#?}", &ast);

    let _ = Interpreter::block(ctx.clone(), &ast);
    /* println!("{:#?}", result);
    println!("{:#?}", ctx); */
}

pub fn stmts(ctx: Rc<RefCell<Context>>, p: &mut Parser, s: &str) {
    p.feed(s);
    p.skip_ws();  // go to the first meaningful item
    let ast = p.module_root_stmts().expect("invalid or no AST. (please turn on RUST_BACKTRACE=1 and report the logs to us!)");

    // DONT LEAVE THIS IN PROD DUMBFUCK
    // println!("ast: {:#?}", &ast);

    let _ = Interpreter::block(
        ctx.clone(),
        &Block {
            stmts: ast,
        }
    );
    /* println!("{:#?}", result);
    println!("{:#?}", ctx); */
}
