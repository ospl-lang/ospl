use super::Parser;
use crate::{
    Expr, Statement, Value
};

impl Parser {
    pub fn print(&mut self) -> Option<Statement> {
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

    pub fn assignment(&mut self) -> Option<Statement> {
        let id: String = self.identifier()?;
        self.skip_ws();
        self.expect_char('=')?;
        self.skip_ws();
        let rhs: Expr = self.expr()?;

        return Some(Statement::Assign {
            left: Box::new(
                Expr::Variable(
                    Box::new(
                        Expr::Literal(
                            Value::String(id)
                        )
                    )
                )
            ),
            right: Box::new(rhs)
        });
    }

    pub fn declaration(&mut self) -> Option<Statement> {
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
        if self.peek_or_consume('=') {
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
        if !self.match_next("break ") {
            return None
        }

        self.skip_ws();
        let ret: Expr = self.expr()?;
        return Some(Statement::Break(ret));
    }

    pub fn continue_statement(&mut self) -> Option<Statement> {
        if !self.match_next("continue ") {
            return None
        }

        self.skip_ws();
        return Some(Statement::Continue);
    }
}