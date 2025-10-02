use super::*;
use crate::Type;

impl Parser {
    pub fn typedef(&mut self) -> Option<Type> {
        return Some(
            if      self.match_next("byte") {Type::Byte}
            else if self.match_next("BYTE") {Type::SignedByte}
            else if self.match_next("word") {Type::Word}
            else if self.match_next("WORD") {Type::SignedWord}
            else if self.match_next("dword") {Type::DoubleWord}
            else if self.match_next("DWORD") {Type::SignedDoubleWord}
            else if self.match_next("qword") {Type::QuadrupleWord}
            else if self.match_next("QWORD") {Type::SignedQuadrupleWord}
            else if self.match_next("half") {Type::Half}
            else if self.match_next("single") {Type::Single}
            else if self.match_next("float") {Type::Float}
            else if self.match_next("bool") {Type::Bool}
            else if self.match_next("str") {Type::String}
            else if self.match_next("tuple") {Type::Tuple}
            else if self.match_next("mixmap") {Type::Mixmap}
            else if self.match_next("object") {Type::Object}
            else if self.match_next("class") {Type::Class}
            else if self.match_next("?!CFFI_Symbol") {Type::ForeignSymbol}
            else if self.match_next("?!CFFI_Function") {Type::ForeignFn}
            else if self.match_next("?!CFFI_Library") {Type::ForeignLib}
            else {return None}
        )
    }
}