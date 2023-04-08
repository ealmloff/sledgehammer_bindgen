use std::collections::HashSet;
use std::ops::Deref;

use super::numbers::Number;
use proc_macro::TokenStream;
use quote::__private::{Span, TokenStream as TokenStream2};
use quote::quote;
use syn::{
    parse::Parse, parse_macro_input, Expr, GenericArgument, Ident, Lit, Pat, PathArguments, Type,
};
use syn::{ForeignItemFn, ItemFn, TypeParamBound};

#[derive(Clone, Debug)]
pub struct Writable {
    size_type: Number,
}

impl Writable {
    fn to_tokens(&self) -> TokenStream2 {
        quote! { impl sledgehammer_utils::Writable }
    }

    fn min_size(&self) -> usize {
        self.size_type.size()
    }

    fn js(&self, parameter: String, read: &mut Read) -> String {
        let s = format!(
            "{}=s.substring(sp,sp+={});",
            parameter,
            self.size_type.js_get(read),
        );
        read.pos += self.min_size();
        s
    }

    fn js_inlined(&self, parameter: String) -> String {
        format!(
            "{}=s.substring(sp,sp+={});",
            parameter,
            self.size_type.js_get_inlined(),
        )
    }

    fn js_get(&self, read: &mut Read) -> String {
        format!("s.substring(sp,sp+={})", self.size_type.js_get(read))
    }

    fn js_get_inlined(&self) -> String {
        format!("s.substring(sp,sp+={})", self.size_type.js_get_inlined())
    }

    fn encode(&self, name: &Ident) -> TokenStream2 {
        let len = Ident::new("len", Span::call_site());
        let write_len = self.size_type.encode(&len);
        quote! {
            unsafe {
                let prev_len = self.str_buffer.len();
                #name.write(&mut self.str_buffer);
                // the length of the string is the change in length of the string buffer
                let #len: usize = std::str::from_utf8_unchecked(&self.str_buffer[prev_len..]).chars().map(|c| c.len_utf16()).sum();
                #write_len
            }
        }
    }
}
