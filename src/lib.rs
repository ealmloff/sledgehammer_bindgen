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
                let ty = if let Type::Path(TypePath { path, .. }) = &*ty.ty {
                    let segments: Vec<_> = path.segments.iter().collect();
                    if let &[simple] = segments.as_slice() {
                        simple.ident.clone()
                    } else {
                        panic!("only simple types are supported")
                    }
                } else {
                    panic!("only simple types are supported")
                };
                (ident, ty)
            }
        });

        Ok(Self {
            name,
            args: Vec::new(),
            body: Vec::new(),
        })
    }
}
