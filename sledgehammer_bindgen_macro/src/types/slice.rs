use super::numbers::{NumberEncoder, NumberEncoderFactory};
use crate::encoder::{CreateEncoder, Encode, EncodeTraitObject, Encoder, Encoders};

use quote::__private::{Span, TokenStream as TokenStream2};
use quote::quote;
use syn::parse_quote;
use syn::{Ident, Type};

pub struct SliceFactory<const N: u32, const S: u32, const STATIC: bool>;

impl<const N: u32, const S: u32, const STATIC: bool> CreateEncoder for SliceFactory<N, S, STATIC> {
    type Output = Slice<N, S, STATIC>;

    fn create(&self, encoder: &mut Encoders) -> Self::Output {
        let array = encoder
            .get_or_insert_with(NumberEncoderFactory::<N>)
            .clone();
        let len = encoder
            .get_or_insert_with(NumberEncoderFactory::<S>)
            .clone();
        let ptr = encoder
            .get_or_insert_with(NumberEncoderFactory::<4>)
            .clone();
        Slice { array, len, ptr }
    }

    fn rust_ident(&self) -> Ident {
        Ident::new(format!("slice{}", N).as_str(), Span::call_site())
    }
}

pub struct Slice<const N: u32, const S: u32, const STATIC: bool> {
    array: EncodeTraitObject,
    len: EncodeTraitObject,
    ptr: EncodeTraitObject,
}

impl<const N: u32, const S: u32, const STATIC: bool> Slice<N, S, STATIC> {
    fn array(&self) -> &NumberEncoder<N> {
        self.array.downcast()
    }
}

impl<const N: u32, const S: u32, const STATIC: bool> Encoder for Slice<N, S, STATIC> {
    fn rust_type(&self) -> Type {
        parse_quote!(())
    }

    fn rust_ident(&self) -> Ident {
        Ident::new(format!("slice{}", N).as_str(), Span::call_site())
    }
}

impl<const N: u32, const S: u32, const STATIC: bool> Encode for Slice<N, S, STATIC> {
    fn encode_js(&self) -> String {
        let ptr_read = self.ptr.encode_js();
        let len_read = self.len.encode_js();
        if STATIC {
            match N {
                1 => format!("new Uint8Array(this.m.buffer,{},{})", ptr_read, len_read),
                2 => format!("new Uint16Array(this.m.buffer,{},{})", ptr_read, len_read),
                4 => format!("new Uint32Array(this.m.buffer,{},{})", ptr_read, len_read),
                _ => todo!(),
            }
        } else {
            let array_ptr = self.array().pointer_js();
            let array_read = self.array().js_ident();
            let slice_end = format!("{array_ptr}+{len_read}");
            format!("(()=>{{this.e={slice_end};const final_array = {array_read}.slice({array_ptr},this.e);{array_ptr}=this.e;return final_array;}})()")
        }
    }

    fn encode_rust(&self, ident: &Ident) -> TokenStream2 {
        let len = Ident::new("__len", Span::call_site());
        let encode_len = self.len.encode_rust(&len);
        let ptr = Ident::new("__ptr", Span::call_site());
        let encode_ptr = self.ptr.encode_rust(&ptr);
        let write_array = (!STATIC).then(|| {
            let item = Ident::new("_i", Span::call_site());
            let encode_array = self.array.encode_rust(&item);
            quote! {
                for &#item in #ident {
                    #encode_array
                }
            }
        });
        let write_ptr = match STATIC {
            true => quote! {
                let #ptr = #ident.as_ptr() as u32;
                #encode_ptr
            },
            false => {
                quote! {}
            }
        };
        quote! {
            #write_ptr
            let #len = #ident.len() as u32;
            #encode_len
            #write_array
        }
    }
}
