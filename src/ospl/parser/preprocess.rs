use std::{fs::File, io::Read};
use crate::Statement;

use super::Parser;

impl Parser {
    fn string_literal(&mut self) -> Option<String> {
        if let Some(raw) = self.raw_string_literal() {
            return Some(raw.into_id())
        } else if let Some(escaped) = self.escaped_string_literal() {
            return Some(escaped.into_id())
        } else {
            return None
        }
    }

    pub fn process_preprocessor_directive(&mut self, ast: &mut Vec<Statement>) -> bool {
        // try usefile
        if let Some(b) = self.attempt(Self::includefile_directive) {
            ast.extend(b);
            return true
        }

        return false
    }

    pub fn includefile_directive(&mut self) -> Option<Vec<Statement>> {
        if !self.match_next("includefile ") {
            return None
        }

        // get the path to the file
        let path = self.string_literal()
            .expect("invalid file path");

        // This is VERY FUCKING DANGEROUS... too bad!
        let mut file = File::open(path)
            .expect("failed to include file (failed to open it, does the file exist?)");

        // read our file
        let mut buffer: String = String::new();
        file.read_to_string(&mut buffer)
            .expect("failed to read file");

        // parse our new file
        let mut include_parser = Parser::new();
        include_parser.feed(&buffer);

        let stmts = include_parser.module_root_stmts()
            .expect("expected valid statements for file");

        return Some(stmts);
    }
}