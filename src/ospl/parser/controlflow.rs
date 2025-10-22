use crate::{SpannedExpr, Subspec};

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

        // classic weird "two-style" blocks around the codebase.
        // yada yada yada you can put a single statement or a block
        let block: Block =
            // multi-style
            if let Some(multi_style) = self.attempt(Self::block) {
                multi_style
            }

            // single-style
            else if let Some(single_style) = self.attempt(Self::stmt) {
                Block {
                    stmts: vec![single_style],
                }
            }

            else {
                self.parse_error("you don't know how a case statement works...")
            };

        return Some((spec, block))
    }

    fn parse_cases_list(&mut self) -> (SpannedExpr, Vec<(Vec<Subspec>, Block)>) {
        self.skip_ws();
        let matching = self.expr()
            .unwrap_or_else(|| self.parse_error("`check` or `select` requires something to match on"));

        self.skip_ws();
        self.expect_char('{')
            .unwrap_or_else(|| self.parse_error("expected opening brace"));

        let mut cases: Vec<(Vec<Subspec>, Block)> = Vec::new();
        loop {
            self.skip_ws();
            match self.peek() {
                Some('}') => {
                    self.pos += 1;  // consume closing brace
                    break;
                },
                Some(',') => {
                    self.pos += 1;
                    continue;
                }
                Some(_) => {
                    cases.push(
                        self.parse_case()
                            .unwrap_or_else(|| self.parse_error("expected case, found something else")
                        )
                    );
                },
                _ => self.parse_error("unexpected EOF parsing `check` or `select` statement")
            }
        }

        return (matching, cases);
    }

    pub fn parse_check(&mut self) -> Option<SpannedStatement> {
        if !self.match_next("check ") {
            return None
        }

        let (matching, cases) = self.parse_cases_list();

        return Some(
            SpannedStatement::new(
                self.lineno,
                Statement::Check {
                    matching,
                    cases
                },
                self.filename.clone()
            )
        )
    }

    pub fn parse_select(&mut self) -> Option<SpannedStatement> {
        if !self.match_next("select ") {
            return None
        }

        let (matching, cases) = self.parse_cases_list();

        return Some(
            SpannedStatement::new(
                self.lineno,
                Statement::Select {
                    matching,
                    cases
                },
                self.filename.clone()
            )
        )
    }

    pub fn parse_loop(&mut self) -> Option<SpannedStatement> {
        if !self.match_next("loop") {
            return None
        }

        self.skip_ws();
        let ret: Block = self.block()
            .unwrap_or_else(|| self.parse_error("a loop requires a body"));
        return Some(
            SpannedStatement::new(
                self.lineno,
                Statement::Loop(
                    Box::new(ret)
                ),
                self.filename.clone()
            )
        )
    }
}