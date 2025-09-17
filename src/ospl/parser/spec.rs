use super::Parser;
use crate::{
    Subspec
};

impl Parser {
    pub fn subspec(&mut self) -> Option<Subspec> {
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
        else if self.peek() == Some('(') {
            // it's a destruction
            return Some(
                Subspec::Destruct(
                    self.spec_def()
                        .unwrap_or_else(|| self.parse_error("expected spec definition, found something else!"))
                )
            )
        }

        else { None }
    }

    pub fn spec_def(&mut self) -> Option<Vec<Subspec>> {
        self.expect_char('(')?;

        let mut spec: Vec<Subspec> = Vec::new();
        loop {
            self.skip_ws();
            match self.peek() {
                Some(')') => {
                    self.pos += 1;
                    break;
                },
                Some(',') => {
                    // consume separator and continue parsing next subspec
                    self.pos += 1;
                    continue;
                },
                Some(_) => {
                    // parse a subspec entry (e.g., identifier or destruct)
                    let value = self.subspec()?;
                    spec.push(value);
                }
                _ => self.parse_error("unexpected EOF in spec definition")
            }
        }

        return Some(spec)
    }
}