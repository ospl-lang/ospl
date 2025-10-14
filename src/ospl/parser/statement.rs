use super::Parser;
use crate::{
    Block, Expr, Statement, Value
};

impl Parser {
    pub fn print(&mut self) -> Option<Statement> {
        self.skip_ws();
        if !self.match_next("print ") {
            return None
        }

        self.skip_ws();
        let ex: Expr = self.expr()?;

        return Some(Statement::Print {
            thing: Box::new(ex)
        });
    }

    pub fn assignment(&mut self) -> Option<Statement> {
        let id = self.expr()?;
        self.skip_ws();
        self.expect_char('=')?;
        self.skip_ws();
        let rhs: Expr = self.expr()?;

        return Some(Statement::Assign {
            left: Box::new(
                /* Expr::Variable(
                    Box::new(
                        Expr::Literal(
                            Value::String(id)
                        )
                    )
                ) */
               id
            ),
            right: Box::new(rhs)
        });
    }

    pub fn declaration(&mut self) -> Option<Statement> {
        // all declarations start with def, if it doesn't,
        // this clearly isn't a declaration.
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
        if self.peek_or_consume('=') {
            self.skip_ws();
            initializer = Some(
                self.expr()
                    .unwrap_or_else(|| self.parse_error("expected valid expression for declaration initializer"))
            );
            self.skip_ws();
        }

        // construct a statement
        return Some(Statement::Declaration {
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

    pub fn return_statement(&mut self) -> Option<Statement> {
        // use "return " instead of "return" because it's a keyword
        if !self.match_next("return ") {
            return None
        }

        self.skip_ws();
        let ret: Expr = self.expr()?;
        return Some(Statement::Return(ret));
    }

    pub fn break_statement(&mut self) -> Option<Statement> {
        if !self.match_next("break") {
            return None
        }

        return Some(
            Statement::Break
        );
    }

    pub fn continue_statement(&mut self) -> Option<Statement> {
        if !self.match_next("continue") {
            return None
        }

        self.skip_ws();
        return Some(Statement::Continue);
    }

    pub fn if_statement(&mut self) -> Option<Statement> {
        if !self.match_next("if ") {
            return None
        }
        
        // followed by condition
        self.skip_ws();
        let condition: Expr = self.expr()
            .unwrap_or_else(|| self.parse_error("if statement requires condition"));

        // followed by on true block
        // TWO WAYS OF DOING TRUE BLOCKS. EITHER YOU HAVE ONE STATEMENT OR A
        // WHOLE BLOCK. WE IMPLEMENT BOTH HERE (sry 4 caps keyboard broke)
        self.skip_ws();
        let on_true: Block =
            // multiple ones style
            if let Some(multi_style) = self.attempt(Self::block) {
                multi_style
            }

            // single ones style
            else if let Some(single_style) = self.attempt(Self::stmt) {
                Block(
                    vec![single_style]
                )
            }
            
            // oops the person is stoopid
            else {
                panic!("if statement requires a true block")
            };

        // support else blocks
        self.skip_ws();
        let on_false: Option<Block> = self.else_statement();

        return Some(Statement::If {
            condition,
            on_true,
            on_false
        })
    }

    pub fn else_statement(&mut self) -> Option<Block> {
        if !self.match_next("else ") {
            return None
        }

        // else statements can be written two different ways.
        // either as a block, like `else {...}`
        self.skip_ws();
        if let Some(block_way) = self.attempt(Self::block) {
            return Some(block_way)
        }

        // or with a single statement, like `else print "hi"`
        else if let Some(stmt_way) = self.attempt(Self::stmt) {
            return Some(
                Block(
                    vec![stmt_way]
                )
            )
        }

        // otherwise, this isn't a valid else clause
        self.parse_error("invalid else clause");
    }

    pub fn import_lib(&mut self) -> Option<Statement> {
        if !self.match_next("import ") {
            return None;
        }

        self.skip_ws();
        let lib_name_value = self.raw_string_literal()
            .unwrap_or_else(|| self.parse_error("expected library name as raw string"));
        let lib_name = lib_name_value.into_id();

        self.skip_ws();
        let lib_path_value = self.raw_string_literal()
            .unwrap_or_else(|| self.parse_error("expected library path as raw string"));
        let lib_path = lib_path_value.into_id();

        Some(Statement::ImportLib { name: lib_name, path: lib_path })
    }

    pub fn bad_idea(&mut self) -> Option<Statement> {
        if !self.match_next("OSPL_memcpy ") {
            return None;
        }

        self.skip_ws();
        let address = self.expr()
            .unwrap_or_else(|| self.parse_error("expected valid expression for address"));

        self.skip_ws();
        self.expect_char(',');
        self.skip_ws();

        let value = self.expr()
            .unwrap_or_else(|| self.parse_error("expected valid expression for value"));

        return Some(
            Statement::BadIdea {
                address,
                value
            }
        );
    }
}