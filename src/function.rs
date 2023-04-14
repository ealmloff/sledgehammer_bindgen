use crate::types::{numbers::NumberEncoderFactory, slice::SliceFactory, string::StrEncoderFactory};
use std::ops::Deref;

use quote::{__private::TokenStream as TokenStream2, quote};
use syn::{parse_quote, Expr, GenericArgument, Ident, ItemFn, Lit, Pat, PathArguments, Type};

use crate::{
    builder::BindingBuilder,
    encoder::{Encode, EncodeTraitObject, Encoders},
    types::numbers,
};

pub struct FunctionBinding {
    name: Ident,
    type_encodings: Vec<TypeEncoding>,
    encoding_order: Vec<usize>,
    js_output: String,
    pub(crate) variables: Vec<String>,
}

struct TypeEncoding {
    ty: Type,
    ident: Ident,
    decode_js: String,
    encode_rust: TokenStream2,
}

impl TypeEncoding {
    fn new(ident: &Ident, ty: Type, encoder: &mut EncodeTraitObject) -> Self {
        Self {
            ty,
            ident: ident.clone(),
            decode_js: encoder.encode_js(),
            encode_rust: encoder.encode_rust(ident),
        }
    }
}

impl FunctionBinding {
    pub fn new(encoders: &mut Encoders, builder: &mut BindingBuilder, function: ItemFn) -> Self {
        let name = function.sig.ident;
        let mut myself = Self {
            name,
            type_encodings: Vec::new(),
            encoding_order: Vec::new(),
            variables: Vec::new(),
            js_output: String::new(),
        };

        for arg in function.sig.inputs {
            match arg {
                syn::FnArg::Receiver(_) => todo!("handle self"),
                syn::FnArg::Typed(ty) => {
                    let ident = if let Pat::Ident(i) = &*ty.pat {
                        &i.ident
                    } else {
                        panic!("only simple idents are supported")
                    };
                    myself.add(encoders, builder, ident, ty.ty.deref().clone())
                }
            }
        }

        let body = if let &[syn::Stmt::Expr(Expr::Lit(lit))] = &function.block.stmts.as_slice() {
            if let Lit::Str(s) = &lit.lit {
                s.value()
            } else {
                panic!("missing body")
            }
        } else {
            panic!("missing body")
        };

        let should_inline = {
            let mut inlined_count = 0;
            parse_js_body(&body, |_| inlined_count += 1);
            inlined_count == myself.type_encodings.len()
        };

        let body = if should_inline {
            // We need to reorder the javascript_decodings to match the order of the parameters
            parse_js_body(&body, |parameter| {
                let idx = myself
                    .type_encodings
                    .iter()
                    .position(|e| e.ident == parameter)
                    .unwrap_or_else(|| {
                        panic!("attempted to inline an unknown parameter: {}", parameter)
                    });
                let encoding = &myself.type_encodings[idx];
                *parameter = encoding.decode_js.clone();
                myself.encoding_order.push(idx);
            })
        } else {
            myself.encoding_order = (0..myself.type_encodings.len()).collect();

            let javascript_decodings: Vec<_> = myself
                .type_encodings
                .iter()
                .map(|encoding| (encoding.ident.to_string(), encoding.decode_js.clone()))
                .collect();

            myself.variables = javascript_decodings
                .iter()
                .map(|(k, _)| k.to_string())
                .collect();

            let unmatched_decodings: String = javascript_decodings
                .into_iter()
                .map(|(k, v)| format!("{}={};", k, v))
                .collect();

            myself.js_output = unmatched_decodings;

            parse_js_body(&body, |_| {})
        };

        myself.js_output += &body;

        myself
    }

    pub fn js(&self) -> &str {
        &self.js_output
    }

    pub fn to_tokens(&self, index: u8) -> TokenStream2 {
        let name = &self.name;
        let args = self.type_encodings.iter().map(|encoding| &encoding.ident);
        let types = self.type_encodings.iter().map(|encoding| &encoding.ty);
        let encode = self.encoding_order.iter().map(|idx| {
            let encoding = &self.type_encodings[*idx];
            &encoding.encode_rust
        });

        quote! {
            #[allow(clippy::uninit_vec)]
            pub fn #name(&mut self, #(#args: #types),*) {
                self.encode_op(#index);
                #(#encode)*
            }
        }
    }

    fn add(
        &mut self,
        encoders: &mut Encoders,
        builder: &mut BindingBuilder,
        ident: &Ident,
        ty: Type,
    ) {
        if let Type::Path(segments) = &ty {
            let segments: Vec<_> = segments.path.segments.iter().collect();
            if let &[simple] = segments.as_slice() {
                let as_str = simple.ident.to_string();
                let encoder = match as_str.as_str() {
                    "u8" => {
                        encoders.get_or_insert_with(numbers::NumberEncoderFactory::<1>, builder)
                    }
                    "u16" => {
                        encoders.get_or_insert_with(numbers::NumberEncoderFactory::<2>, builder)
                    }
                    "u32" => {
                        encoders.get_or_insert_with(numbers::NumberEncoderFactory::<4>, builder)
                    }
                    _ => panic!("unsupported type"),
                };

                let type_encoding = TypeEncoding::new(ident, ty, encoder);
                self.type_encodings.push(type_encoding);
                return;
            }
        } else if let Type::Reference(ty_ref) = &ty {
            let static_str = ty_ref
                .lifetime
                .as_ref()
                .filter(|l| l.ident == "static")
                .is_some();
            if let Type::Path(segments) = &*ty_ref.elem {
                let ty = if static_str {
                    parse_quote!(&'static str)
                } else {
                    parse_quote!(&str)
                };
                let segments: Vec<_> = segments.path.segments.iter().collect();
                if let &[simple] = segments.as_slice() {
                    let as_str = simple.ident.to_string().to_lowercase();
                    if as_str == "str" {
                        let mut cache = None;
                        if let PathArguments::AngleBracketed(gen) = &simple.arguments {
                            let generics: Vec<_> = gen.args.iter().collect();
                            if let GenericArgument::Type(Type::Path(t)) = &generics[0] {
                                let segments: Vec<_> = t.path.segments.iter().collect();
                                if let &[simple] = segments.as_slice() {
                                    if let Some(GenericArgument::Type(Type::Path(t))) =
                                        &generics.get(1)
                                    {
                                        let segments: Vec<_> = t.path.segments.iter().collect();
                                        if let &[simple] = segments.as_slice() {
                                            cache = Some(simple.ident.clone());
                                        }
                                    }
                                    let encoder = match simple.ident.to_string().as_str() {
                                        "u8" => {
                                            encoders.insert(NumberEncoderFactory::<1>, builder);
                                            encoders.get_or_insert_with(
                                                StrEncoderFactory::<1> {
                                                    cache_name: cache,
                                                    static_str,
                                                },
                                                builder,
                                            )
                                        }
                                        "u16" => {
                                            encoders.insert(NumberEncoderFactory::<2>, builder);
                                            encoders.get_or_insert_with(
                                                StrEncoderFactory::<2> {
                                                    cache_name: cache,
                                                    static_str,
                                                },
                                                builder,
                                            )
                                        }
                                        "u32" => {
                                            encoders.insert(NumberEncoderFactory::<4>, builder);
                                            encoders.get_or_insert_with(
                                                StrEncoderFactory::<4> {
                                                    cache_name: cache,
                                                    static_str,
                                                },
                                                builder,
                                            )
                                        }
                                        _ => panic!("unsupported type"),
                                    };

                                    let type_encoding = TypeEncoding::new(ident, ty, encoder);
                                    self.type_encodings.push(type_encoding);
                                    return;
                                }
                            }
                        }
                        encoders.insert(NumberEncoderFactory::<4>, builder);
                        let encoder = encoders.get_or_insert_with(
                            StrEncoderFactory::<4> {
                                cache_name: cache,
                                static_str,
                            },
                            builder,
                        );

                        let type_encoding = TypeEncoding::new(ident, ty, encoder);
                        self.type_encodings.push(type_encoding);
                        return;
                    }
                }
            }
            if let Type::Slice(slice) = &*ty_ref.elem {
                if !static_str {
                    return;
                }
                if let Type::Path(segments) = &*slice.elem {
                    let segments: Vec<_> = segments.path.segments.iter().collect();
                    if let &[simple] = segments.as_slice() {
                        let as_str = simple.ident.to_string();
                        if let PathArguments::AngleBracketed(gen) = &simple.arguments {
                            let generics: Vec<_> = gen.args.iter().collect();
                            if let &[GenericArgument::Type(Type::Path(t))] = generics.as_slice() {
                                let segments: Vec<_> = t.path.segments.iter().collect();
                                if let &[simple] = segments.as_slice() {
                                    let encoder = match simple.ident.to_string().as_str() {
                                        "u8" => match as_str.as_str() {
                                            "u8" => {
                                                encoders.insert(NumberEncoderFactory::<1>, builder);
                                                encoders.get_or_insert_with(
                                                    SliceFactory::<1, 1>,
                                                    builder,
                                                )
                                            }
                                            "u16" => {
                                                encoders.insert(NumberEncoderFactory::<2>, builder);
                                                encoders.get_or_insert_with(
                                                    SliceFactory::<2, 1>,
                                                    builder,
                                                )
                                            }
                                            "u32" => {
                                                encoders.insert(NumberEncoderFactory::<4>, builder);
                                                encoders.get_or_insert_with(
                                                    SliceFactory::<4, 1>,
                                                    builder,
                                                )
                                            }
                                            _ => panic!("unsupported type"),
                                        },
                                        "u16" => match as_str.as_str() {
                                            "u8" => {
                                                encoders.insert(NumberEncoderFactory::<1>, builder);
                                                encoders.get_or_insert_with(
                                                    SliceFactory::<1, 2>,
                                                    builder,
                                                )
                                            }
                                            "u16" => {
                                                encoders.insert(NumberEncoderFactory::<2>, builder);
                                                encoders.get_or_insert_with(
                                                    SliceFactory::<2, 2>,
                                                    builder,
                                                )
                                            }
                                            "u32" => {
                                                encoders.insert(NumberEncoderFactory::<4>, builder);
                                                encoders.get_or_insert_with(
                                                    SliceFactory::<4, 2>,
                                                    builder,
                                                )
                                            }
                                            _ => panic!("unsupported type"),
                                        },
                                        "u32" => match as_str.as_str() {
                                            "u8" => {
                                                encoders.insert(NumberEncoderFactory::<1>, builder);
                                                encoders.get_or_insert_with(
                                                    SliceFactory::<1, 4>,
                                                    builder,
                                                )
                                            }
                                            "u16" => {
                                                encoders.insert(NumberEncoderFactory::<2>, builder);
                                                encoders.get_or_insert_with(
                                                    SliceFactory::<2, 4>,
                                                    builder,
                                                )
                                            }
                                            "u32" => {
                                                encoders.insert(NumberEncoderFactory::<4>, builder);
                                                encoders.get_or_insert_with(
                                                    SliceFactory::<4, 4>,
                                                    builder,
                                                )
                                            }
                                            _ => panic!("unsupported type"),
                                        },
                                        _ => panic!("unsupported type"),
                                    };

                                    let type_encoding = TypeEncoding::new(ident, ty, encoder);
                                    self.type_encodings.push(type_encoding);
                                    return;
                                }
                            }
                        } else {
                            let encoder = match as_str.as_str() {
                                "u8" => {
                                    encoders.insert(NumberEncoderFactory::<1>, builder);
                                    encoders.get_or_insert_with(SliceFactory::<1, 4>, builder)
                                }
                                "u16" => {
                                    encoders.insert(NumberEncoderFactory::<2>, builder);
                                    encoders.get_or_insert_with(SliceFactory::<2, 4>, builder)
                                }
                                "u32" => {
                                    encoders.insert(NumberEncoderFactory::<4>, builder);
                                    encoders.get_or_insert_with(SliceFactory::<4, 4>, builder)
                                }
                                _ => panic!("unsupported type"),
                            };
                            let type_encoding = TypeEncoding::new(ident, ty, encoder);
                            self.type_encodings.push(type_encoding);
                            return;
                        }
                    }
                }
            }
        }
        panic!("unsupported type")
    }
}

fn parse_js_body(s: &str, mut f: impl FnMut(&mut String)) -> String {
    let mut inside_param = false;
    let mut last_was_escape = false;
    let mut current_param = String::new();
    let mut current_text = String::new();
    for c in s.chars() {
        match c {
            '\\' => last_was_escape = true,
            '$' => {
                if last_was_escape {
                    if inside_param {
                        current_param.push(c);
                    } else {
                        current_text.push(c);
                    }
                    last_was_escape = false;
                } else {
                    if inside_param {
                        // segments.push((current_segment, current_param));
                        f(&mut current_param);
                        current_text += &current_param;
                        current_param = String::new();
                    }
                    inside_param = !inside_param;
                }
            }
            _ => {
                last_was_escape = false;
                if inside_param {
                    current_param.push(c);
                } else {
                    current_text.push(c);
                }
            }
        }
    }
    current_text
}

#[test]
fn replace_vars() {
    let output = parse_js_body("hello $world$ this is $a$ test", |s| {
        *s = s.to_uppercase();
    });

    assert_eq!(output, "hello WORLD this is A test");
}
