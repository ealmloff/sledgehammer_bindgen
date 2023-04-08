use quote::{__private::TokenStream as TokenStream2, quote};
use syn::{Expr, Ident, Type};

use crate::{select_bits_js, select_bits_js_inner};

pub struct BindingBuilder {
    js_u32_count: usize,
    js_flag_count: usize,
}

impl BindingBuilder {
    fn new() -> Self {
        Self {
            js_u32_count: 0,
            js_flag_count: 0,
        }
    }

    pub fn u32(&mut self) -> RustJSU32 {
        let id = self.js_u32_count;
        self.js_u32_count += 1;
        RustJSU32 { id }
    }

    pub fn flag(&mut self) -> RustJSFlag {
        let id = self.js_flag_count;
        self.js_flag_count += 1;
        RustJSFlag { id }
    }
}

pub struct RustJSU32 {
    id: usize,
}

impl RustJSU32 {
    pub fn read_js(&self) -> String {
        format!("u32s[{}]", self.id)
    }

    pub fn write_rust(&self, value: Expr) -> TokenStream2 {
        let id = self.id;

        quote! {
            u32[#id] = #value;
        }
    }
}

pub struct RustJSFlag {
    id: usize,
}

impl RustJSFlag {
    pub fn read_js(&self) -> String {
        select_bits_js_inner("flags", 32, self.id, 1)
    }

    pub fn write_rust(&self, value: Expr) -> TokenStream2 {
        let id = self.id;

        quote! {
            if #value {
                flags |= 1 << #id;
            } else {
                flags &= !(1 << #id);
            }
        }
    }
}
