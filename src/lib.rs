use proc_macro::TokenStream;
use quote::quote;
use syn::{parse::Parse, parse_macro_input, Ident, LitStr, Pat, PatType, Type, TypePath};

/// #[bindgen]
/// extern "C" {
///    fn foo(i: i32, s: &str){
///       "
///       function x(){}
///       "
///    }
///    fn bar(){
///    }
/// }
#[proc_macro_attribute]
pub fn bindgen(args: TokenStream, input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as Bindings);

    // TokenStream::from(quote!(#input))
    todo!("return")
}

struct Bindings {
    functions: Vec<FunctionBinding>,
}

impl Parse for Bindings {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        let extren_block = syn::ItemForeignMod::parse(input)?;

        let functions = extren_block
            .items
            .into_iter()
            .map(|item| match item {
                syn::ForeignItem::Verbatim(s) => syn::parse2(s).unwrap(),
                _ => panic!("only functions are supported"),
            })
            .collect();

        Ok(Bindings { functions })
    }
}

struct FunctionBinding {
    name: Ident,
    args: Vec<(Ident, Ident)>,
    body: Vec<LitStr>,
}

impl Parse for FunctionBinding {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        let function = syn::ItemFn::parse(input)?;
        let name = function.sig.ident;
        let args = function.sig.inputs.iter().map(|arg| match arg {
            syn::FnArg::Receiver(_) => todo!("self"),
            syn::FnArg::Typed(ty) => {
                let ident = if let Pat::Ident(i) = &*ty.pat {
                    i.ident.clone()
                } else {
                    panic!("only simple idents are supported")
                };
                (ident, SupportedTypes::from(&*ty.ty))
            }
        });

        Ok(Self {
            name,
            args: Vec::new(),
            body: Vec::new(),
        })
    }
}

enum SupportedTypes {
    Number(Number),
    Slice(Slice),
    Str,
}

impl<'a> From<&'a Type> for SupportedTypes {
    fn from(ty: &'a Type) -> Self {
        if let Type::Path(segments) = ty {
            let segments: Vec<_> = segments.path.segments.iter().collect();
            if let &[simple] = segments.as_slice() {
                let as_str = simple.ident.to_string();
                return SupportedTypes::Number(match as_str.as_str() {
                    "u8" => Number::U8,
                    "u16" => Number::U16,
                    "u32" => Number::U32,
                    "i8" => Number::I8,
                    "i16" => Number::I16,
                    "i32" => Number::I32,
                    _ => panic!("unsupported type"),
                });
            }
        } else {
            if let Type::Reference(ty) = ty {
                if let Type::Slice(slice) = &*ty.elem {
                    if let Type::Path(segments) = &*slice.elem {
                        let segments: Vec<_> = segments.path.segments.iter().collect();
                        if let &[simple] = segments.as_slice() {
                            let as_str = simple.ident.to_string();
                            if as_str == "str" {
                                return SupportedTypes::Str;
                            }
                            return SupportedTypes::Slice(match as_str.as_str() {
                                "u8" => Slice::U8,
                                "u16" => Slice::U16,
                                "u32" => Slice::U32,
                                "i8" => Slice::I8,
                                "i16" => Slice::I16,
                                "i32" => Slice::I32,
                                _ => panic!("unsupported type"),
                            });
                        }
                    }
                }
            }
        }
        panic!("unsupported type")
    }
}

enum Slice {
    U8,
    U16,
    U32,
    I8,
    I16,
    I32,
}

enum Number {
    U8,
    U16,
    U32,
    I8,
    I16,
    I32,
}
