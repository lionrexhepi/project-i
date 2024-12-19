use crate::{ast, ir, lexer};

pub enum ErrorPayload {
    Lexer(lexer::Error),
    Ast(ast::Error),
    Ir(ir::Error),
}

pub struct Error {
    pub location: SourceLocation,
    pub payload: ErrorPayload,
}

pub struct SourceLocation {
    pub file: String,
    pub line: usize,
    pub column: usize,
}

pub trait CodeError {
    fn wrap(self, location: SourceLocation) -> Error;
}

impl CodeError for lexer::Error {
    fn wrap(self, location: SourceLocation) -> Error {
        Error {
            location,
            payload: ErrorPayload::Lexer(self),
        }
    }
}

impl CodeError for ast::Error {
    fn wrap(self, location: SourceLocation) -> Error {
        Error {
            location,
            payload: ErrorPayload::Ast(self),
        }
    }
}

impl CodeError for ir::Error {
    fn wrap(self, location: SourceLocation) -> Error {
        Error {
            location,
            payload: ErrorPayload::Ir(self),
        }
    }
}
