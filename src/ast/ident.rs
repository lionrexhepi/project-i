use std::{borrow::Borrow, ops::Deref};

use smol_str::SmolStr;

#[derive(Debug, PartialEq, Eq, Clone, Hash)]
pub struct Identifier(SmolStr);

impl From<SmolStr> for Identifier {
    fn from(s: SmolStr) -> Self {
        Identifier(s)
    }
}

impl From<&str> for Identifier {
    fn from(s: &str) -> Self {
        Identifier(s.into())
    }
}

impl From<String> for Identifier {
    fn from(s: String) -> Self {
        Identifier(s.into())
    }
}

impl From<Identifier> for SmolStr {
    fn from(ident: Identifier) -> Self {
        ident.0
    }
}

impl<'s> From<&'s Identifier> for &'s str {
    fn from(ident: &Identifier) -> &str {
        ident.0.as_str()
    }
}

impl AsRef<str> for Identifier {
    fn as_ref(&self) -> &str {
        &self.0
    }
}

impl Borrow<str> for Identifier {
    fn borrow(&self) -> &str {
        &self.0
    }
}

impl Deref for Identifier {
    type Target = str;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl std::fmt::Display for Identifier {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl Identifier {
    pub fn new<S: AsRef<str>>(s: S) -> Self {
        Identifier::from(s.as_ref())
    }

    pub fn as_str(&self) -> &str {
        &self.0
    }
}
