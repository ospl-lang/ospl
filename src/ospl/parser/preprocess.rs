use crate::ospl::SpannedStatement;

use super::Parser;

impl Parser {
    pub fn string_literal(&mut self) -> Option<String> {
        if let Some(raw) = self.raw_string_literal() {
            return Some(raw.into_id())
        } else if let Some(escaped) = self.escaped_string_literal() {
            return Some(escaped.into_id())
        } else {
            return None
        }
    }

    pub fn process_preprocessor_directive(&mut self, ast: &mut Vec<SpannedStatement>) -> bool {
        // try usefile
        if let Some(b) = self.attempt(Self::includefile_directive) {
            ast.extend(b);
            return true
        }

        return false
    }

    pub fn includefile_directive(&mut self) -> Option<Vec<SpannedStatement>> {
        if !self.match_next("includefile ") {
            return None
        }

        // get the path to the file
        let path: String = self.string_literal()
            .expect("invalid file path");

        // parse our new file
        let mut include_parser: Parser = self.subparser(&path);

        let stmts: Vec<SpannedStatement> = include_parser.module_root_stmts()
            .expect("expected valid statements for file");

        return Some(stmts);
    }
}