use quote::__private::TokenStream as TokenStream2;
use quote::quote;
use syn::parse_quote;
use syn::Ident;
use syn::Type;

use crate::builder::{RustJSFlag, RustJSU32};
use crate::encoder::*;

pub struct NumberEncoder<const S: u32> {
    array_moved_flag: RustJSFlag,
    array_ptr: RustJSU32,
}

impl<const S: u32> NumberEncoder<S> {
    pub fn new(builder: &mut crate::builder::BindingBuilder) -> Self {
        let array_moved_flag = builder.flag();
        let array_ptr = builder.u32();
        Self {
            array_moved_flag,
            array_ptr,
        }
    }

    fn size(&self) -> u32 {
        match S {
            1 => 8,
            2 => 16,
            4 => 32,
            _ => panic!("Invalid number size"),
        }
    }
}

impl<const S: u32> CreateEncoder for NumberEncoder<S> {
    fn new(builder: &mut crate::builder::BindingBuilder) -> Self {
        let array_moved_flag = builder.flag();
        let array_ptr = builder.u32();
        Self {
            array_moved_flag,
            array_ptr,
        }
    }
}

impl<const S: u32> Encoder for NumberEncoder<S> {
    fn global_js(&self) -> String {
        let size = self.size();
        format!("let u{size}buf,u{size}bufp;")
    }

    fn pre_run_js(&self) -> String {
        let moved = self.array_moved_flag.read_js();
        let ptr = self.array_ptr.read_js();
        let size = self.size();
        format!(
            "if ({moved}){{
                u{size}buf=new Uint{size}Array(m.buffer,{ptr})
            }}
            u{size}bufp=0;"
        )
    }

    fn rust_type(&self) -> Type {
        match S {
            1 => parse_quote! {Vec<u8>},
            2 => parse_quote! {Vec<u16>},
            4 => parse_quote! {Vec<u32>},
            _ => panic!("Invalid number size"),
        }
    }

    fn rust_ident(&self) -> Ident {
        match S {
            1 => parse_quote! {buf_u8},
            2 => parse_quote! {buf_u16},
            4 => parse_quote! {buf_u32},
            _ => panic!("Invalid number size"),
        }
    }

    fn global_rust(&self) -> TokenStream2 {
        quote! {}
    }

    fn pre_run_rust(&self) -> TokenStream2 {
        let ident = self.rust_ident();
        quote! {
            #ident.clear();
        }
    }

    fn post_run_rust(&self) -> TokenStream2 {
        let ident = self.rust_ident();
        quote! {
            #ident.clear();
        }
    }
}

impl Encode for NumberEncoder<1> {
    fn encode_js(&self) -> String {
        "u8buf.getUint8(u8bufp++,true)".to_string()
    }

    fn encode_rust(&self, ident: &Ident) -> TokenStream2 {
        quote! {
            self.buf_u8.push(#ident);
        }
    }
}

impl Encode for NumberEncoder<2> {
    fn encode_js(&self) -> String {
        "u8buf.getUint16(u16bufp++,true)".to_string()
    }

    fn encode_rust(&self, ident: &Ident) -> TokenStream2 {
        quote! {
            self.buf_u16.push(#ident);
        }
    }
}

impl Encode for NumberEncoder<4> {
    fn encode_js(&self) -> String {
        "u8buf.getUint32(u32bufp++,true)".to_string()
    }

    fn encode_rust(&self, ident: &Ident) -> TokenStream2 {
        quote! {
            self.buf_u32.push(#ident);
        }
    }
}
