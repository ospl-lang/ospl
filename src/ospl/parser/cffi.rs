use super::Parser;
use crate::ospl::Expr;

impl Parser {
    pub fn cffi_load(&mut self) -> Option<Expr> {
        if !self.match_next("cffi_load ") {
            return None
        }

        // abusing into_id is kind of bad here but I don't care.
        // TODO: make a proper function and don't spam into_id()
        let library = self.raw_string_literal()?.into_id();

        eprintln!("loading library: '{}'", library);

        return Some(Expr::CffiLoad(library))
    }
}