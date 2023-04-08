use quote::{__private::TokenStream as TokenStream2, quote};
use syn::{Expr, Ident, Type};

use crate::{builder::BindingBuilder, select_bits_js, select_bits_js_inner};

mod numbers;
// mod slice;
// mod string;
// mod writable;

pub trait CreateEncoder {
    fn new(builder: &mut BindingBuilder) -> Self;
}

pub trait Encoder {
    fn global_js(&self) -> String {
        String::new()
    }

    fn pre_run_js(&self) -> String {
        String::new()
    }

    fn rust_type(&self) -> Type;

    fn rust_ident(&self) -> Ident;

    fn global_rust(&self) -> TokenStream2;

    fn pre_encode_rust(&self) -> TokenStream2 {
        quote!()
    }
}

pub trait Encodable {
    type Encoder: CreateEncoder + Encoder;

    fn encode_js(&self) -> String;

    fn encode_rust(&self, ident: Ident) -> TokenStream2;
}

// impl<'a> From<&'a Type> for SupportedTypes {
//     fn from(ty: &'a Type) -> Self {
//         if let Type::Path(segments) = ty {
//             let segments: Vec<_> = segments.path.segments.iter().collect();
//             if let &[simple] = segments.as_slice() {
//                 let as_str = simple.ident.to_string();
//                 return SupportedTypes::SupportedTypes(SupportedTypes::Number(
//                     match as_str.as_str() {
//                         "u8" => Number::U8,
//                         "u16" => Number::U16,
//                         "u32" => Number::U32,
//                         _ => panic!("unsupported type"),
//                     },
//                 ));
//             }
//         } else if let Type::Reference(ty) = ty {
//             if let Type::Path(segments) = &*ty.elem {
//                 let segments: Vec<_> = segments.path.segments.iter().collect();
//                 if let &[simple] = segments.as_slice() {
//                     let as_str = simple.ident.to_string();
//                     if as_str == "str" {
//                         if let PathArguments::AngleBracketed(gen) = &simple.arguments {
//                             let generics: Vec<_> = gen.args.iter().collect();
//                             if let GenericArgument::Type(Type::Path(t)) = &generics[0] {
//                                 let segments: Vec<_> = t.path.segments.iter().collect();
//                                 if let &[simple] = segments.as_slice() {
//                                     let mut cache = None;
//                                     if let Some(GenericArgument::Type(Type::Path(t))) =
//                                         &generics.get(1)
//                                     {
//                                         let segments: Vec<_> = t.path.segments.iter().collect();
//                                         if let &[simple] = segments.as_slice() {
//                                             cache = Some(simple.ident.clone());
//                                         }
//                                     }
//                                     return SupportedTypes::SupportedTypes(SupportedTypes::Str(
//                                         Str {
//                                             size_type: match simple.ident.to_string().as_str() {
//                                                 "u8" => Number::U8,
//                                                 "u16" => Number::U16,
//                                                 "u32" => Number::U32,
//                                                 _ => panic!("unsupported type"),
//                                             },
//                                             cache_name: cache,
//                                             static_str: ty
//                                                 .lifetime
//                                                 .as_ref()
//                                                 .filter(|l| l.ident == "static")
//                                                 .is_some(),
//                                         },
//                                     ));
//                                 }
//                             }
//                         }
//                         return SupportedTypes::SupportedTypes(SupportedTypes::Str(Str {
//                             size_type: Number::U32,
//                             cache_name: None,
//                             static_str: ty
//                                 .lifetime
//                                 .as_ref()
//                                 .filter(|l| l.ident == "static")
//                                 .is_some(),
//                         }));
//                     }
//                 }
//             }
//             if let Type::Slice(slice) = &*ty.elem {
//                 if let Type::Path(segments) = &*slice.elem {
//                     let segments: Vec<_> = segments.path.segments.iter().collect();
//                     if let &[simple] = segments.as_slice() {
//                         let as_str = simple.ident.to_string();
//                         if let PathArguments::AngleBracketed(gen) = &simple.arguments {
//                             let generics: Vec<_> = gen.args.iter().collect();
//                             if let &[GenericArgument::Type(Type::Path(t))] = generics.as_slice() {
//                                 let segments: Vec<_> = t.path.segments.iter().collect();
//                                 if let &[simple] = segments.as_slice() {
//                                     let size = match simple.ident.to_string().as_str() {
//                                         "u8" => Number::U8,
//                                         "u16" => Number::U16,
//                                         "u32" => Number::U32,
//                                         _ => panic!("unsupported type"),
//                                     };
//                                     return SupportedTypes::SupportedTypes(SupportedTypes::Slice(
//                                         Slice {
//                                             size,
//                                             inner: match as_str.as_str() {
//                                                 "u8" => Number::U8,
//                                                 "u16" => Number::U16,
//                                                 "u32" => Number::U32,
//                                                 _ => panic!("unsupported type"),
//                                             },
//                                         },
//                                     ));
//                                 }
//                             }
//                         } else {
//                             return SupportedTypes::SupportedTypes(SupportedTypes::Slice(Slice {
//                                 size: Number::U32,
//                                 inner: match as_str.as_str() {
//                                     "u8" => Number::U8,
//                                     "u16" => Number::U16,
//                                     "u32" => Number::U32,
//                                     _ => panic!("unsupported type"),
//                                 },
//                             }));
//                         }
//                     }
//                 }
//             }
//         } else if let Type::ImplTrait(tr) = ty {
//             let traits: Vec<_> = tr.bounds.iter().collect();
//             if let &[TypeParamBound::Trait(tr)] = traits.as_slice() {
//                 let segments: Vec<_> = tr.path.segments.iter().collect();
//                 if let &[simple] = segments.as_slice() {
//                     if simple.ident == "Writable" {
//                         if let PathArguments::AngleBracketed(gen) = &simple.arguments {
//                             let generics: Vec<_> = gen.args.iter().collect();
//                             if let &[GenericArgument::Type(Type::Path(t))] = generics.as_slice() {
//                                 let segments: Vec<_> = t.path.segments.iter().collect();
//                                 if let &[simple] = segments.as_slice() {
//                                     let size = match simple.ident.to_string().as_str() {
//                                         "u8" => Number::U8,
//                                         "u16" => Number::U16,
//                                         "u32" => Number::U32,
//                                         _ => panic!("unsupported type"),
//                                     };
//                                     return SupportedTypes::SupportedTypes(
//                                         SupportedTypes::Writable(Writable { size_type: size }),
//                                     );
//                                 }
//                             }
//                         } else {
//                             return SupportedTypes::SupportedTypes(SupportedTypes::Writable(
//                                 Writable {
//                                     size_type: Number::U32,
//                                 },
//                             ));
//                         }
//                     }
//                 }
//             }
//         }
//         panic!("unsupported type")
//     }
// }
