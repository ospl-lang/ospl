use crate::Subspec;

use super::*;

impl Parser {
    pub fn parse_case(&mut self) -> Option<(Vec<Subspec>, Block)> {
        // make sure it's ACTUALLY a case
        if !self.match_next("case ") {
            return None
        }

        self.skip_ws();
        let spec = self.spec_def()
            .unwrap_or_else(|| self.parse_error("expected a spec"));

        self.skip_ws();
        let block = self.block()
            .unwrap_or_else(|| self.parse_error("expected a block"));

        return Some((spec, block))
    }

    pub fn parse_check(&mut self) -> Option<Statement> {
        if !self.match_next("check ") {
            return None
        }

        self.skip_ws();
        let matching = self.expr()
            .unwrap_or_else(|| self.parse_error("check requires something to match on"));

        self.skip_ws();
        self.expect_char('{')
            .unwrap_or_else(|| self.parse_error("expected opening brace"));

        let mut cases: Vec<(Vec<Subspec>, Block)> = Vec::new();
        loop {
            self.skip_ws();
            match self.peek() {
                Some('}') => break,
                Some(_) => {
                    cases.push(
                        self.parse_case()
                            .unwrap_or_else(|| self.parse_error("expected case, found something else"))
                    );
                },
                _ => self.parse_error("unexpected EOF parsing `check` statement")
            }
        }

        return Some(
            Statement::Check {
                matching: Box::new(matching),
                cases
            }
        )
    }
}