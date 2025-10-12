use super::Parser;
use crate::{
    Subspec
};

impl Parser {
    pub fn subspec(&mut self) -> Option<Subspec> {
        // ==== BASE CASE(s) ====
        // ==== VALUE BIND ====
        if let Some(id) = self.attempt(Self::identifier) {
            // check if there is a type annotation
            if self.peek_or_consume(':') {
                self.skip_ws();
                let target_typ = self.typedef()
                    .unwrap_or_else(|| self.parse_error("expected valid type identifier in typed ref bind (after `:`)"));

                return Some(
                    Subspec::BindTyped(
                        id, target_typ
                    )
                );
            }

            // we do a binding otherwise
            return Some(Subspec::Bind(id))
        }

        // ==== REF BIND ====
        else if self.peek_or_consume('$') {
            // we bind by reference
            let Some(id) = self.attempt(Self::identifier)
            else {
                self.parse_error("expected an identifier after ref bind");
            };

            // check if there is a type annotation next to it
            if self.peek_or_consume(':') {
                self.skip_ws();
                let target_typ = self.typedef()
                    .unwrap_or_else(|| self.parse_error("expected valid type identifier in typed ref bind (after `:`)"));

                return Some(
                    Subspec::BindRefTyped(
                        id, target_typ
                    )
                );
            }

            return Some(Subspec::BindRef(id));
        }

        // ==== RECURSIVE CASES ====
        else if self.peek() == Some('(') {
            // it's a destruction
            return Some(
                Subspec::Destruct(
                    self.spec_def()
                        .unwrap_or_else(|| self.parse_error("expected spec definition, found something else!"))
                )
            )
        }

        // ==== MORE FUCKING BASE CASES ====
        // ==== LITERAL EXPECTATION ====
        else if let Some(lit) = self.attempt(Self::literal) {
            return Some(
                Subspec::Literal(
                    lit.into_value()
                        .unwrap_or_else(|| self.parse_error("literal failed"))
                )
            )
        }

        // ==== allow nothing ====
        else if self.peek_or_consume('_') {
            return Some(
                Subspec::Ignore
            )
        }

        // ==== take self ====
        else if self.peek_or_consume('?') {
            return Some(
                Subspec::ThisRef(
                    self.identifier()
                        .unwrap_or_else(|| self.parse_error("expected identifier for ThisRef"))
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