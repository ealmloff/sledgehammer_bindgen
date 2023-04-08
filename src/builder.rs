use quote::{__private::TokenStream as TokenStream2, quote};
use syn::Expr;

use crate::select_bits_js_inner;

#[derive(Default)]
pub struct BindingBuilder {
    js_u32_count: usize,
    js_flag_count: usize,
    js_var_count: usize,
}

impl BindingBuilder {
    pub fn uniuqe_var(&mut self) -> String {
        let old = self.js_var_count;
        self.js_var_count += 1;
        format!("v{}", old)
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
    
    pub fn u32s_type_rust(&self) -> TokenStream2{
        let len = self.js_u32_count;
        quote! {
            [u32; #len]
        }
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
            self.u32s[#id] = #value;
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
