use std::{
    cell::RefCell,
    char,
    collections::VecDeque,
    fs,
    io::{BufReader, Read},
    path::{Path, PathBuf},
};

use snafu::ResultExt;

use crate::errors::SourceLocation;

use super::{File, InvalidUtf8Snafu, IoSnafu, ReadCharResult, Result, UnexpectedEofSnafu};

pub struct DiskFile {
    name: PathBuf,
    file: RefCell<BufReader<fs::File>>,
    buffer: RefCell<VecDeque<char>>,
    location: SourceLocation,
}

impl DiskFile {
    const BUF_CHARS: usize = 255;
    const BUF_BYTES: usize = Self::BUF_CHARS * size_of::<char>();

    pub fn open(path: impl AsRef<Path>) -> Result<Self> {
        let path = path.as_ref();
        Ok(Self {
            name: path.to_path_buf(),
            file: RefCell::new(BufReader::new(fs::File::open(path).with_context(|_| {
                IoSnafu {
                    file: path.display().to_string(),
                }
            })?)),
            buffer: Default::default(),
            location: SourceLocation::new(path.display()),
        })
    }

    fn rebuffer(&self) -> Result<()> {
        let mut char_buf = [0u8; Self::BUF_BYTES];
        let mut rest = 0; // In case we hit the middle of a unicode sequence with our buffer
        let mut buffer = self.buffer.borrow_mut();
        loop {
            let read = self
                .file
                .borrow_mut()
                .read(&mut char_buf[rest..])
                .with_context(|_| IoSnafu {
                    file: Some(self.name.display().to_string()),
                })?;

            if read == 0 {
                break Ok(());
            }

            match std::str::from_utf8(&char_buf) {
                Ok(str) => {
                    buffer.extend(str.chars());
                    if buffer.len() >= Self::BUF_CHARS {
                        break Ok(());
                    }
                }
                Err(err) if err.error_len().is_some() => {
                    let valid_bytes = err.valid_up_to();
                    buffer.extend(
                        unsafe { std::str::from_utf8_unchecked(&char_buf[..valid_bytes]) }.chars(),
                    );
                    rest = char_buf.len() - valid_bytes;

                    if rest > size_of::<char>() {
                        let bad_bytes =
                            char_buf[valid_bytes..(valid_bytes + 4)].try_into().unwrap(); // The range length is 4
                        break Err(err).context(InvalidUtf8Snafu {
                            seq: u32::from_le_bytes(bad_bytes),
                        });
                    }

                    let mut new_buf = [0u8; Self::BUF_BYTES];
                    new_buf[..rest].copy_from_slice(&char_buf[rest..]);
                    char_buf = new_buf;
                }
                Err(eof) => break Err(eof).context(UnexpectedEofSnafu),
            }
        }
    }
}

impl File for DiskFile {
    fn advance(&mut self) -> ReadCharResult {
        if self.buffer.borrow().is_empty() {
            self.rebuffer()?;
        }

        Ok(self.buffer.borrow_mut().pop_front())
    }

    fn peek(&self) -> ReadCharResult {
        if self.buffer.borrow().is_empty() {
            self.rebuffer()?;
        }

        Ok(self.buffer.borrow().front().copied())
    }

    fn peek_n(&self, n: usize) -> ReadCharResult {
        if self.buffer.borrow().len() < n {
            self.rebuffer()?;
        }

        Ok(self.buffer.borrow().get(n).copied())
    }

    fn location(&self) -> &SourceLocation {
        &self.location
    }
}

#[cfg(test)]
mod literal {
    use super::*;

    pub struct Literal {
        data: VecDeque<char>,
        location: SourceLocation,
    }

    impl File for Literal {
        fn advance(&mut self) -> ReadCharResult {
            let c = self.data.remove(0);
            match c {
                Some('\n') => {
                    self.location.advance_line();
                }
                Some(_) => self.location.advance_col(),
                None => {}
            }
            Ok(c)
        }

        fn peek(&self) -> ReadCharResult {
            Ok(self.data.front().copied())
        }

        fn peek_n(&self, n: usize) -> ReadCharResult {
            Ok(self.data.get(n).copied())
        }

        fn location(&self) -> &SourceLocation {
            &self.location
        }
    }

    impl FromIterator<char> for Literal {
        fn from_iter<I: IntoIterator<Item = char>>(iter: I) -> Self {
            Literal {
                data: iter.into_iter().collect(),
                location: SourceLocation::new("<string literal>"),
            }
        }
    }
}

#[cfg(test)]
pub use literal::*;
