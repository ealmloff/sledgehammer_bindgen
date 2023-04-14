use super::numbers::{NumberEncoder, NumberEncoderFactory};
use crate::encoder::{CreateEncoder, Encode, Encoder};

use quote::__private::{Span, TokenStream as TokenStream2};
use quote::quote;
use syn::parse_quote;
use syn::{Ident, Type};

pub struct SliceFactory<const N: u32, const S: u32>;

impl<const N: u32, const S: u32> CreateEncoder for SliceFactory<N, S> {
    type Output = Slice<N, S>;

    fn create(&self, builder: &mut crate::builder::BindingBuilder) -> Self::Output {
        let len = NumberEncoderFactory::<S>.create(builder);
        let ptr = NumberEncoderFactory::<4>.create(builder);
        Slice { len, ptr }
    }

    fn rust_ident(&self) -> Ident {
        Ident::new(format!("slice{}", N).as_str(), Span::call_site())
    }
}

pub struct Slice<const N: u32, const S: u32> {
    len: NumberEncoder<S>,
    ptr: NumberEncoder<4>,
}

impl<const N: u32, const S: u32> Encoder for Slice<N, S> {
    fn rust_type(&self) -> Type {
        parse_quote!(())
    }

    fn rust_ident(&self) -> Ident {
        Ident::new(format!("slice{}", N).as_str(), Span::call_site())
    }
}

impl<const N: u32, const S: u32> Encode for Slice<N, S> {
    fn encode_js(&self) -> String {
        let ptr_read = self.ptr.encode_js();
        let len_read = self.len.encode_js();
        match N {
            1 => format!("new Uint8Array(m.buffer,{},{})", ptr_read, len_read),
            2 => format!("new Uint16Array(m.buffer,{},{})", ptr_read, len_read),
            4 => format!("new Uint32Array(m.buffer,{},{})", ptr_read, len_read),
            _ => todo!(),
        }
    }

    fn encode_rust(&self, ident: &Ident) -> TokenStream2 {
        let len = Ident::new("len", Span::call_site());
        let encode_len = self.len.encode_rust(&len);
        let ptr = Ident::new("ptr", Span::call_site());
        let encode_ptr = self.ptr.encode_rust(&ptr);
        quote! {
            let #ptr = #ident.as_ptr() as u32;
            #encode_ptr
            let #len = #ident.len() as u32;
            #encode_len
        }
    }
}
