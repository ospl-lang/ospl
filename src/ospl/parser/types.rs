use super::*;
use crate::Type;

impl Parser {
    const OSPL_CFFI_SYMBOL_TYPE: &str = "OSPL_CFFI_Symbol";
    const OSPL_CFFI_LIBRARY_TYPE: &str = "OSPL_CFFI_Library";
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
            else if self.match_next("mix") {Type::Mixmap}
            else if self.match_next("obj") {Type::Object}
            else if self.match_next(Self::OSPL_CFFI_SYMBOL_TYPE) {Type::ForeignSymbol}
            else if self.match_next(Self::OSPL_CFFI_FN_KW) {Type::ForeignFn}
            else if self.match_next(Self::OSPL_CFFI_LIBRARY_TYPE) {Type::ForeignLib}
            else {return None}
        )
    }
}