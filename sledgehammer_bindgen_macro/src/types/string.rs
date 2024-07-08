use quote::__private::{Span, TokenStream as TokenStream2};
use quote::quote;
use syn::{parse_quote, Ident, Type};

use crate::builder::{RustJSFlag, RustJSU32};
use crate::encoder::{CreateEncoder, Encode, EncodeTraitObject, Encoder, Encoders};

use super::numbers::{NumberEncoder, NumberEncoderFactory};

const CACHE_MISS_BIT: u32 = 1 << 7;

pub struct GeneralStringFactory;

pub struct GeneralString {
    str_moved_flag: RustJSFlag,
    str_tiny_flag: RustJSFlag,
    str_used_flag: RustJSFlag,
    str_ptr: RustJSU32,
    len: RustJSU32,
}

impl CreateEncoder for GeneralStringFactory {
    type Output = GeneralString;

    fn create(&self, encoder: &mut Encoders) -> GeneralString {
        let builder = encoder.builder();
        let str_moved_flag = builder.flag();
        let str_tiny_flag = builder.flag();
        let str_used_flag = builder.flag();
        let str_ptr = builder.u32();
        let len = builder.u32();
        GeneralString {
            str_moved_flag,
            str_tiny_flag,
            str_used_flag,
            str_ptr,
            len,
        }
    }

    fn rust_ident(&self) -> Ident {
        parse_quote!(str_buffer)
    }
}

impl Encode for GeneralString {
    fn encode_js(&self) -> String {
        todo!()
    }

    fn encode_rust(&self, _: &Ident) -> TokenStream2 {
        todo!()
    }
}

impl Encoder for GeneralString {
    fn rust_type(&self) -> Type {
        parse_quote! {Vec<u8>}
    }

    fn rust_ident(&self) -> Ident {
        parse_quote! {str_buffer}
    }

    fn initializer(&self) -> String {
        "this.s = \"\";this.lsp = null;this.sp = null;this.sl = null;this.c = new TextDecoder();"
            .to_string()
    }

    fn pre_run_js(&self) -> String {
        let moved = self.str_moved_flag.read_js();
        let used = self.str_used_flag.read_js();
        let tiny = self.str_tiny_flag.read_js();
        let ptr = self.str_ptr.read_js();
        let len = self.len.read_js();
        format!(
            r#"if ({moved}){{
                this.lsp = {ptr};
            }}
            if ({used}) {{
                this.sl = {len};
                if ({tiny}) {{
                    this.sp = this.lsp;
                    this.s = "";
                    this.e = this.sp + ((this.sl / 4) | 0) * 4;
                    while (this.sp < this.e) {{
                        this.t = this.m.getUint32(this.sp, true);
                        this.s += String.fromCharCode(
                            this.t & 255,
                            (this.t & 65280) >> 8,
                            (this.t & 16711680) >> 16,
                            this.t >> 24
                        );
                        this.sp += 4;
                    }}
                    while (this.sp < this.lsp + this.sl) {{
                        this.s += String.fromCharCode(this.m.getUint8(this.sp++));
                    }}
                }} else {{
                    let buffer = new Uint8Array(this.m.buffer, this.lsp, this.sl);
                    // If the wasm buffer is a shared array buffer, we need to copy the data out before decoding https://github.com/DioxusLabs/dioxus/issues/2589
                    // Note: We intentionally don't use instanceof here because SharedArrayBuffer can be created even when SharedArrayBuffer is not defined...
                    if (this.m.buffer.constructor.name === "SharedArrayBuffer") {{
                        let arrayBuffer = new ArrayBuffer(this.sl);
                        new Uint8Array(arrayBuffer).set(buffer);
                        buffer = arrayBuffer;
                    }}
                    this.s = this.c.decode(buffer);
                }}
            }}
            this.sp=0;"#
        )
    }

    fn init_rust(&self) -> TokenStream2 {
        self.str_moved_flag.write_rust(parse_quote!(true))
    }

    fn memory_moved_rust(&self) -> TokenStream2 {
        self.init_rust()
    }

    fn pre_run_rust(&self) -> TokenStream2 {
        let ident = <Self as Encoder>::rust_ident(self);
        let write_ptr = self
            .str_ptr
            .write_rust(parse_quote!(self.#ident.as_ptr() as u32));
        let write_small = self
            .str_tiny_flag
            .write_rust(parse_quote!(self.#ident.len() < 100 && self.str_buffer.is_ascii()));
        let write_used = self
            .str_used_flag
            .write_rust(parse_quote!(!self.str_buffer.is_empty()));
        let len = self.len.write_rust(parse_quote!(self.#ident.len() as u32));
        let read_ptr = self.str_ptr.get_rust();
        let moved = self
            .str_moved_flag
            .write_rust(parse_quote!(#read_ptr != self.#ident.as_ptr() as u32));

        quote! {
            if !self.str_buffer.is_empty() {
                #moved
                #write_small
                #write_ptr
                #len
            }
            #write_used
        }
    }

    fn post_run_rust(&self) -> TokenStream2 {
        let ident = <Self as Encoder>::rust_ident(self);
        quote! {
            self.#ident.clear();
        }
    }

    fn merge_memory_rust(&self) -> TokenStream2 {
        let ident = <Self as Encoder>::rust_ident(self);
        let write_ptr = self.str_ptr.write_rust(parse_quote!(current_ptr));
        quote! {
            {
                #write_ptr
                current_ptr += self.#ident.len() as u32;
                self.#ident.iter().copied()
            }
        }
    }
}

pub struct StrEncoder<const S: u32> {
    size_type: EncodeTraitObject,
    cache_name: Option<(Ident, EncodeTraitObject)>,
    static_str: bool,
}

impl<const S: u32> StrEncoder<S> {
    fn size_type(&self) -> &NumberEncoder<S> {
        self.size_type.downcast()
    }
}

pub struct StrEncoderFactory<const S: u32> {
    pub cache_name: Option<Ident>,
    pub static_str: bool,
}

impl<const S: u32> CreateEncoder for StrEncoderFactory<S> {
    type Output = StrEncoder<S>;

    fn create(&self, encoder: &mut Encoders) -> Self::Output {
        StrEncoder {
            size_type: encoder.get_or_insert_with(NumberEncoderFactory::<S>),
            cache_name: self
                .cache_name
                .clone()
                .map(|name| (name, encoder.get_or_insert_with(NumberEncoderFactory::<1>))),
            static_str: self.static_str,
        }
    }

    fn rust_ident(&self) -> Ident {
        if let Some(cache) = &self.cache_name {
            cache.clone()
        } else {
            Ident::new(&format!("str_cache{}", S * 8), Span::call_site())
        }
    }
}

impl<const S: u32> Encoder for StrEncoder<S> {
    fn rust_type(&self) -> Type {
        if self.cache_name.is_some() {
            if self.static_str {
                parse_quote! {
                    sledgehammer_utils::ConstLru<*const str, NonHashBuilder, 128, 256>
                }
            } else {
                parse_quote! {
                    sledgehammer_utils::LruCache
                }
            }
        } else {
            parse_quote! {()}
        }
    }

    fn rust_ident(&self) -> Ident {
        if let Some((cache, _)) = &self.cache_name {
            cache.clone()
        } else {
            Ident::new(&format!("str_cache{}", S * 8), Span::call_site())
        }
    }

    fn initializer(&self) -> String {
        match &self.cache_name {
            Some((cache, encoder)) => {
                let get_u8 = encoder.encode_js();

                let cache_idx_mask = !CACHE_MISS_BIT;
                let read_string_length = self.size_type.encode_js();
                format!(
                    "this.{cache} = [];
                    this.{cache}_cache_hit = null;
                    this.{cache}_cache_idx;
                    this.get_{cache} = function() {{
                        this.{cache}_cache_idx = {get_u8};
                        if(this.{cache}_cache_idx & {CACHE_MISS_BIT}){{
                            this.{cache}_cache_hit=this.s.substring(this.sp,this.sp+={read_string_length});
                            this.{cache}[this.{cache}_cache_idx&{cache_idx_mask}]=this.{cache}_cache_hit;
                            return this.{cache}_cache_hit;
                        }}
                        else{{
                            return this.{cache}[this.{cache}_cache_idx&{cache_idx_mask}];
                        }}
                    }};",
                )
            }
            None => "".to_string(),
        }
    }
}

impl<const S: u32> Encode for StrEncoder<S> {
    fn encode_js(&self) -> String {
        match &self.cache_name {
            Some((cache, _)) => format!("this.get_{cache}()"),
            None => format!(
                "this.s.substring(this.sp,this.sp+={})",
                self.size_type.encode_js()
            ),
        }
    }

    fn encode_rust(&self, name: &Ident) -> TokenStream2 {
        let len = Ident::new("__len", Span::call_site());
        let char_len = Ident::new("__char_len", Span::call_site());
        let write_len = self.size_type.encode_rust(&char_len);
        let char_len_type = self.size_type().element_type();
        let encode = quote! {
            let #len = #name.len();
            let #char_len: usize = #name.chars().map(|c| c.len_utf16()).sum();
            let #char_len = {
                #char_len as #char_len_type
            };
            #write_len
            let old_len = self.str_buffer.len();
            unsafe {
                // SAFETY: We reserve uninitialized memory but then immediately write to it to make it initialized
                self.str_buffer.reserve(#len);
                self.str_buffer.set_len(old_len + #len);
                __copy(#name.as_bytes(), &mut self.str_buffer[old_len..], #len);
            }
        };
        match &self.cache_name {
            Some((cache, size)) => {
                if self.static_str {
                    let write_size = size.encode_rust(&Ident::new("cache_id", Span::call_site()));
                    quote! {
                        let (_id, _new) = self.#cache.push(#name);
                        if _new {
                            let cache_id = #CACHE_MISS_BIT as u8 | _id;
                            #write_size
                            #encode
                        }
                        else {
                            let cache_id = _id;
                            #write_size
                        }
                    }
                } else {
                    let write_size = size.encode_rust(&Ident::new("cache_id", Span::call_site()));
                    quote! {
                        if let Some(&id) = self.#cache.get(#name){
                            let cache_id = id;
                            #write_size
                        }
                        else {
                            let cache_len = self.#cache.len() as u8;
                            let id = if cache_len == #CACHE_MISS_BIT as u8 {
                                if let Some((_, id)) = self.#cache.pop_lru() {
                                    self.#cache.put(#name.to_string(), id);
                                    id
                                }
                                else {
                                    unreachable!()
                                }
                            } else {
                                self.#cache.put(#name.to_string(), cache_len);
                                cache_len
                            };
                            let cache_id =  #CACHE_MISS_BIT as u8 | id;
                            #write_size
                            #encode
                        }
                    }
                }
            }
            None => encode,
        }
    }
}
