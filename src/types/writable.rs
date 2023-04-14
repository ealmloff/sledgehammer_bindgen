use quote::__private::{Span, TokenStream as TokenStream2};
use quote::quote;
use syn::{parse_quote, Ident, Type};

use crate::encoder::{CreateEncoder, Encode, Encoder};

use super::numbers::NumberEncoder;

pub struct WritableEncoder<const S: u32> {
    size_type: NumberEncoder<S>,
}

pub struct WritableEncoderFactory<const S: u32>;

impl<const S: u32> CreateEncoder for WritableEncoderFactory<S> {
    type Output = WritableEncoder<S>;

    fn create(&self, builder: &mut crate::builder::BindingBuilder) -> Self::Output {
        WritableEncoder {
            size_type: NumberEncoder::new(builder),
        }
    }

    fn rust_ident(&self) -> Ident {
        Ident::new(&format!("writable_{}", S * 8), Span::call_site())
    }
}

impl<const S: u32> Encoder for WritableEncoder<S> {
    fn rust_type(&self) -> Type {
        parse_quote! {()}
    }

    fn rust_ident(&self) -> Ident {
        Ident::new(&format!("writable_{}", S * 8), Span::call_site())
    }
}

impl<const S: u32> Encode for WritableEncoder<S> {
    fn encode_js(&self) -> String {
        format!("s.substring(sp,sp+={})", self.size_type.encode_js())
    }

    fn encode_rust(&self, name: &Ident) -> TokenStream2 {
        let char_len = Ident::new("char_len", Span::call_site());
        let write_len = self.size_type.encode_rust(&char_len);
        quote! {
            let prev_len = self.str_buffer.len();
            #name.write(&mut self.str_buffer);
            // the length of the string is the change in length of the string buffer
            let #char_len: usize = unsafe { std::str::from_utf8_unchecked(&self.str_buffer[prev_len..]).chars().map(|c| c.len_utf16()).sum() };
            let #char_len = {
                use std::convert::TryInto;
                #char_len.try_into().unwrap()
            };
            #write_len
        }
    }
}
