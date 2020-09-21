use std::fmt::Display;

use proc_macro2::{Span, TokenStream};

use quote::{quote, quote_spanned};

pub type Result<T> = std::result::Result<T, Error>;
#[derive(Debug)]
pub struct Error {
    message: String,
    span: Option<Span>
}

impl Error {
    pub fn new(message: String, span: Option<Span>) -> Self {
        Self { message, span }
    }

    pub fn with_msg<S: Into<String>>(message: S) -> Error {
        Error::new(message.into(), None)
    }

    pub fn with_msg_spanned<S: Into<String>>(message: S, span: Span) -> Error {
        Error::new(message.into(), Some(span))
    }

    pub fn to_compile_error(&self) -> TokenStream {
        let msg = &self.message;
        if let Some(span) = self.span {
            #[rustfmt::skip]
            quote_spanned! {
		span.into()=> compile_error!(#msg);
            }
        } else {
            #[rustfmt::skip]
            quote! {
		compile_error!(#msg);
            }
        }
    }
}

impl From<(String, Span)> for Error {
    fn from(f: (String, Span)) -> Self {
        Error::with_msg_spanned(f.0, f.1)
    }
}

impl From<(&str, Span)> for Error {
    fn from(f: (&str, Span)) -> Self {
        Error::with_msg_spanned(f.0, f.1)
    }
}

impl From<String> for Error {
    fn from(s: String) -> Self {
        Error::with_msg(s)
    }
}

impl From<&str> for Error {
    fn from(s: &str) -> Self {
        Error::with_msg(s)
    }
}

impl Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(&self.message)
    }
}

impl std::error::Error for Error {}
