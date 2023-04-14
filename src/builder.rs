use quote::{__private::TokenStream as TokenStream2, quote};
use syn::{parse_quote, Expr, Ident};

use crate::select_bits_js_inner;

#[derive(Default)]
pub struct BindingBuilder {
    js_u32_count: usize,
    js_flag_count: usize,
}

impl BindingBuilder {
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

    pub fn rust_ident(&self) -> Ident {
        parse_quote!(metadata)
    }

    pub fn rust_type(&self) -> TokenStream2 {
        let len = self.js_u32_count + 1;
        quote! {
            std::pin::Pin<std::boxed::Box<[std::cell::Cell<u32>; #len]>>
        }
    }

    pub fn rust_init(&self) -> TokenStream2 {
        quote! {
            std::boxed::Box::pin(Default::default())
        }
    }

    pub fn pre_run_js(&self) -> String {
        "metaflags=m.getUint32(d,true);".to_string()
    }
}

pub struct RustJSU32 {
    id: usize,
}

impl RustJSU32 {
    pub fn read_js(&self) -> String {
        format!("m.getUint32(d+{}*4,true)", self.id + 1)
    }

    pub fn write_rust(&self, value: Expr) -> TokenStream2 {
        let id = self.id;

        quote! {
            self.metadata[#id + 1].set(#value);
        }
    }

    pub fn get_rust(&self) -> TokenStream2 {
        let id = self.id;

        quote! {
            self.metadata[#id + 1].get()
        }
    }
}

pub struct RustJSFlag {
    id: usize,
}

impl RustJSFlag {
    pub fn read_js(&self) -> String {
        select_bits_js_inner("metaflags", 32, self.id, 1)
    }

    pub fn write_rust(&self, value: Expr) -> TokenStream2 {
        let id = self.id;

        quote! {
            self.metadata[0].set(if #value { self.metadata[0].get() | (1 << #id) } else { self.metadata[0].get() & !(1 << #id) });
        }
    }
}
