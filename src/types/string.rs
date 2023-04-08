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

#[derive(Clone, Debug)]
pub struct Str {
    size_type: Number,
    cache_name: Option<Ident>,
    static_str: bool,
}

impl Str {
    fn inlinable(&self) -> bool {
        self.cache_name.is_none()
    }

    fn to_tokens(&self) -> TokenStream2 {
        if self.static_str {
            quote! { &'static str }
        } else {
            quote! { &str }
        }
    }

    fn min_size(&self) -> usize {
        self.size_type.size() + usize::from(self.cache_name.is_some())
    }

    fn js(&self, parameter: String, read: &mut Read) -> String {
        match &self.cache_name {
            Some(cache) => {
                let check_cache_hit = format!("(i&{})!=0", 1 << (read.pos * 8 + 7));
                let cache_hit = select_bits_js(read, 7);
                read.pos += 1;
                let string_len = self.size_type.js_get(read);
                format!(
                    "if({}){{{}=s.substring(sp,sp+={});{}[{}]={};}}else{{{}={}[{}];}}",
                    check_cache_hit,
                    parameter,
                    string_len,
                    cache,
                    cache_hit,
                    parameter,
                    parameter,
                    cache,
                    cache_hit,
                )
            }
            None => {
                let s = format!(
                    "{}=s.substring(sp,sp+={});",
                    parameter,
                    self.size_type.js_get(read),
                );
                s
            }
        }
    }

    fn js_get_inlined(&self) -> Option<String> {
        self.cache_name
            .is_none()
            .then(|| format!("s.substring(sp,sp+={})", self.size_type.js_get_inlined()))
    }

    fn encode(&self, name: &Ident) -> TokenStream2 {
        let len = Ident::new("len", Span::call_site());
        let char_len = Ident::new("char_len", Span::call_site());
        let write_len = self.size_type.encode(&char_len);
        let len_byte_size = self.size_type.size();
        let encode = quote! {
            let #len = #name.len();
            let #char_len: usize = #name.chars().map(|c| c.len_utf16()).sum();
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
            Some(cache) => {
                if self.static_str {
                    quote! {
                        let (_id, _new) = #cache.push(#name);
                        if _new {
                            *self.msg.as_mut_ptr().add(self.msg.len()) = 128 | _id;
                            self.msg.set_len(self.msg.len() + 1);
                            #encode
                        }
                        else {
                            *self.msg.as_mut_ptr().add(self.msg.len()) = _id;
                            unsafe {
                                // SAFETY: Increase the length by the size of _id (u8) and the type of the length of the string
                                self.msg.set_len(self.msg.len() + #len_byte_size + 1);
                            }
                        }
                    }
                } else {
                    quote! {
                        if let Some(&id) = #cache.get(#name){
                            *self.msg.as_mut_ptr().add(self.msg.len()) = id;
                            unsafe {
                                // SAFETY: Increase the length by the size of _id (u8) and the type of the length of the string
                                self.msg.set_len(self.msg.len() + #len_byte_size + 1);
                            }
                        }
                        else {
                            let cache_len = #cache.len() as u8;
                            let id = if cache_len == 128 {
                                if let Some((_, id)) = #cache.pop_lru() {
                                    #cache.put(#name.to_string(), id);
                                    id
                                }
                                else {
                                    unreachable!()
                                }
                            } else {
                                #cache.put(#name.to_string(), cache_len);
                                cache_len
                            };
                            *self.msg.as_mut_ptr().add(self.msg.len()) = 128 | id;
                            self.msg.set_len(self.msg.len() + 1);
                            #encode
                        }
                    }
                }
            }
            None => encode,
        }
    }
}
