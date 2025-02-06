use smol_str::SmolStr;

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

#[derive(Debug, Clone)]
pub struct SourceLocation {
    pub file: SmolStr,
    pub line: usize,
    pub column: usize,
}

impl SourceLocation {
    pub fn new(file: impl ToString) -> Self {
        Self {
            file: file.to_string().into(),
            line: 1,
            column: 1,
        }
    }

    pub fn advance_col(&mut self) {
        self.column += 1;
    }

    pub fn advance_line(&mut self) {
        self.column = 1;
        self.line += 1;
    }
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
