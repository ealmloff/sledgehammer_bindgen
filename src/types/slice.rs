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

#[derive(Debug, Clone)]
pub struct Slice {
    size: Number,
    inner: Number,
}

impl Slice {
    fn min_size(&self) -> usize {
        self.size.size() + 4
    }

    fn js_inlined(&self, parameter: String) -> String {
        let ptr_read = Number::U32.js_get_inlined();
        let len_read = match self.size {
            Number::U8 => "m.getUint8(p+=4,true)".to_string(),
            Number::U16 => "m.getUint16(p+=4,true)".to_string(),
            Number::U32 => "m.getUint32(p+=4,true)".to_string(),
            _ => panic!("unsupported length type"),
        };
        let read = match self.inner {
            Number::U8 => format!("new Uint8Array(m.buffer,{},{});", ptr_read, len_read),
            Number::U16 => format!("new Uint16Array(m.buffer,{},{});", ptr_read, len_read),
            Number::U32 => format!("new Uint32Array(m.buffer,{},{});", ptr_read, len_read),
            _ => todo!(),
        };
        parameter + "=" + &read + "p+=" + &self.size.size().to_string() + ";"
    }

    fn to_tokens(&self) -> TokenStream2 {
        let inner = self.inner.to_tokens();
        quote! { &[#inner] }
    }

    fn encode(&self, name: &Ident) -> TokenStream2 {
        let len = Ident::new("len", Span::call_site());
        let encode_len = self.size.encode(&len);
        let ptr = Ident::new("ptr", Span::call_site());
        let encode_ptr = Number::U32.encode(&ptr);
        quote! {
            let #ptr = #name.as_ptr();
            #encode_ptr
            let #len = #name.len();
            #encode_len
        }
    }
}
