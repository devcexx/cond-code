use proc_macro2::{Delimiter, Group, Ident, Literal, Punct, TokenTree};

use crate::error::{Error, Result};

pub trait TokenType: Sized {
    fn match_token(t: TokenTree) -> Option<Self>;
    fn as_str() -> &'static str;
}

impl TokenType for Group {
    fn match_token(t: TokenTree) -> Option<Self> {
        match t {
            TokenTree::Group(token) => Some(token),
            _ => None
        }
    }

    fn as_str() -> &'static str {
        "group"
    }
}

impl TokenType for Ident {
    fn match_token(t: TokenTree) -> Option<Self> {
        match t {
            TokenTree::Ident(token) => Some(token),
            _ => None
        }
    }

    fn as_str() -> &'static str {
        "identifier"
    }
}

impl TokenType for Punct {
    fn match_token(t: TokenTree) -> Option<Self> {
        match t {
            TokenTree::Punct(token) => Some(token),
            _ => None
        }
    }

    fn as_str() -> &'static str {
        "punct"
    }
}

impl TokenType for Literal {
    fn match_token(t: TokenTree) -> Option<Self> {
        match t {
            TokenTree::Literal(token) => Some(token),
            _ => None
        }
    }

    fn as_str() -> &'static str {
        "literal"
    }
}

pub fn require_token<T: TokenType>(it: &mut dyn Iterator<Item = TokenTree>) -> Result<T> {
    if let Some(next) = it.next() {
        if let Some(next) = T::match_token(next) {
            Ok(next)
        } else {
            Err(Error::with_msg(format!(
                "Expected {} but not found",
                T::as_str()
            )))
        }
    } else {
        Err(Error::with_msg(format!(
            "Expected {} but found EOF",
            T::as_str()
        )))
    }
}

pub fn require_punct(it: &mut dyn Iterator<Item = TokenTree>, ch: char) -> Result<Punct> {
    require_token::<Punct>(it).and_then(|punct| {
        if punct.as_char() == ch {
            Ok(punct)
        } else {
            Err(Error::with_msg_spanned(
                format!("Expected {} but {} found", ch, punct.as_char()),
                punct.span()
            ))
        }
    })
}

pub fn require_group(
    it: &mut dyn Iterator<Item = TokenTree>,
    delimiter: Delimiter
) -> Result<Group> {
    require_token::<Group>(it).and_then(|group| {
        if group.delimiter() == delimiter {
            Ok(group)
        } else {
            Err(Error::with_msg_spanned(
                format!("Expected {:?} but {:?} found", delimiter, group.delimiter()),
                group.span()
            ))
        }
    })
}

pub fn require_eof(it: &mut dyn Iterator<Item = TokenTree>) -> Result<()> {
    match it.next() {
        None => Ok(()),
        _ => Err(Error::with_msg("Expected EOF"))
    }
}
