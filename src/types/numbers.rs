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
    pub fn pointer_js(&self) -> String {
        let size = self.size();
        format!("u{size}bufp")
    }

    pub fn size(&self) -> u32 {
        match S {
            1 => 8,
            2 => 16,
            4 => 32,
            _ => panic!("Invalid number size"),
        }
    }

    pub fn element_type(&self) -> Type {
        match S {
            1 => parse_quote! {u8},
            2 => parse_quote! {u16},
            4 => parse_quote! {u32},
            _ => panic!("Invalid number size"),
        }
    }
}

pub struct NumberEncoderFactory<const S: u32>;

impl<const S: u32> CreateEncoder for NumberEncoderFactory<S> {
    type Output = NumberEncoder<S>;

    fn create(&self, encoders: &mut Encoders) -> Self::Output {
        let builder = encoders.builder();
        let array_moved_flag = builder.flag();
        let array_ptr = builder.u32();
        NumberEncoder {
            array_moved_flag,
            array_ptr,
        }
    }

    fn rust_ident(&self) -> Ident {
        match S {
            1 => parse_quote! {U8},
            2 => parse_quote! {U16},
            4 => parse_quote! {U32},
            _ => panic!("Invalid number size"),
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
        let size_in_bytes = size / 8;
        let pointer = self.pointer_js();
        format!(
            "if ({moved}){{
                t = {ptr};
                u{size}buf=new Uint{size}Array(m.buffer,t,((m.buffer.byteLength-t)-(m.buffer.byteLength-t)%{size_in_bytes})/{size_in_bytes});
            }}
            {pointer}=0;"
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
            1 => parse_quote! {u8_arr},
            2 => parse_quote! {u16_arr},
            4 => parse_quote! {u32_arr},
            _ => panic!("Invalid number size"),
        }
    }

    fn init_rust(&self) -> TokenStream2 {
        self.array_moved_flag.write_rust(parse_quote!(true))
    }

    fn memory_moved_rust(&self) -> TokenStream2 {
        self.init_rust()
    }

    fn pre_run_rust(&self) -> TokenStream2 {
        let ident = self.rust_ident();
        let write_ptr = self
            .array_ptr
            .write_rust(parse_quote!(self.#ident.as_ptr() as u32));
        let read_ptr = self.array_ptr.get_rust();
        let moved = self
            .array_moved_flag
            .write_rust(parse_quote!(#read_ptr != self.#ident.as_ptr() as u32));

        quote! {
            #moved
            #write_ptr
        }
    }

    fn post_run_rust(&self) -> TokenStream2 {
        let ident = self.rust_ident();
        quote! {
            self.#ident.clear();
        }
    }

    fn merge_memory_rust(&self) -> TokenStream2 {
        let ident = self.rust_ident();
        let write_ptr = self.array_ptr.write_rust(parse_quote!(current_ptr));
        let add_buffer = (S != 1).then(|| {
            quote! {
                let buffer_size = (#S - current_ptr % #S);
                let zeroed_buffer = std::iter::repeat(0u8).take(buffer_size as usize);
                current_ptr += buffer_size;
            }
        });
        let buffer_memory = quote! { self.#ident.iter().flat_map(|&x| x.to_le_bytes().into_iter()) };
        let final_memory = if S != 1 {
            quote! {
                zeroed_buffer.chain(#buffer_memory)
            }
        } else {
            buffer_memory
        };
        quote! {
            // align the array pointer so that it is a multiple of N
            {
                #add_buffer
                #write_ptr
                current_ptr += self.#ident.len() as u32 * #S;
                #final_memory
            }
        }
    }
}

impl<const S: u32> NumberEncoder<S> {
    pub(crate) fn js_ident(&self) -> String {
        let size = self.size();
        format!("u{size}buf")
    }
}

impl<const S: u32> Encode for NumberEncoder<S> {
    fn encode_js(&self) -> String {
        let size = self.size();
        let pointer = self.pointer_js();
        format!("u{size}buf[{pointer}++]")
    }

    fn encode_rust(&self, ident: &Ident) -> TokenStream2 {
        let rust_ident = self.rust_ident();
        quote! {
            self.#rust_ident.push(#ident);
        }
    }
}
