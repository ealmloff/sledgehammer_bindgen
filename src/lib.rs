use std::collections::{HashMap, HashSet};

use proc_macro::TokenStream;
use quote::__private::TokenStream as TokenStream2;
use quote::{quote, ToTokens};
use syn::{
    parse::Parse, parse_macro_input, Expr, ExprLit, GenericArgument, Ident, Lit, LitStr, Pat,
    PatType, PathArguments, Type, TypePath,
};

#[proc_macro_attribute]
pub fn bindgen(_: TokenStream, input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as Bindings);

    // TokenStream::from(quote!(#input))
    println!("{}", input.js());
    input.to_tokens().into()
}

#[derive(Debug)]
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

#[test]
fn test() {
    assert_eq!(function_discriminant_size_bits(1), 0);
    assert_eq!(function_discriminant_size_bits(2), 1);
    assert_eq!(function_discriminant_size_bits(3), 2);
    assert_eq!(function_discriminant_size_bits(4), 2);
    assert_eq!(function_discriminant_size_bits(5), 3);
    assert_eq!(function_discriminant_size_bits(6), 3);
    assert_eq!(function_discriminant_size_bits(7), 3);
    assert_eq!(function_discriminant_size_bits(8), 3);
    assert_eq!(function_discriminant_size_bits(9), 4);
}

fn function_discriminant_size_bits(function_count: u32) -> usize {
    let len = function_count as u32;
    (32 - len.next_power_of_two().leading_zeros() as usize).saturating_sub(1)
}

fn with_n_1_bits(n: usize) -> u32 {
    (1u64 << n as u64).saturating_sub(1) as u32
}

fn select_bits_js(input: &str, pos: usize, len: usize) -> String {
    if len == 32 {
        assert!(pos == 0);
        return input.to_string();
    }
    assert!(len < 32);
    let mut s = String::new();

    if pos != 0 {
        s += &format!("({} >>> {})", input, pos);
    } else {
        s += input;
    }

    let num = with_n_1_bits(len);
    s += &format!(" & {}", num);

    s
}

impl Bindings {
    fn js(&self) -> String {
        let op_size = function_discriminant_size_bits(self.functions.len() as u32);
        let start = format!(
            r#"
let m, p, lss, sp, d, t, c, s, sl, op, {};
export function init(_d) {{
    d = _d;
    c = new TextDecoder();
}}
export function update_memory(_m) {{
    m = _m;
}}
export function run() {{
    t = m.getUint8(d);
    if (t & 0x01){{
        p = m.getUint32(d + 1);
    }}
    if (t & 0x02){{
        if (t & 0x04){{
            lss = m.getUint32(d + 5);
        }}
        sl = m.getUint32(d + 5);
        if (t & 0x08) {{
            sp = lss;
            s = "";
            endRounded = sp + ((sl / 4) | 0) * 4;
            while (sp < endRounded) {{
                t = m.getUint32(sp);
                s += String.fromCharCode(t >> 24, (t & 0x00FF0000) >> 16, (t & 0x0000FF00) >> 8, (t & 0x000000FF));
                sp += 4;
            }}
            switch (lss + sl - sp) {{
                case 3:
                    t = m.getUint32(sp);
                    s += String.fromCharCode(t >> 24, (t & 0x00FF0000) >> 16, (t & 0x0000FF00) >> 8);
                    break;
                case 2:
                    t = m.getUint16(sp);
                    s += String.fromCharCode(t >> 8, t & 0xFF);
                    break;
                case 1:
                    s += String.fromCharCode(m.getUint8(sp));
                    break;
                case 0:
                    break;
            }}
        }}
        else {{
            s = c.decode(new DataView(m.buffer, lss, sl));
        }}
        sp = 0;
    }}
    for (; ;) {{
        op = this.view.getUint32(p, true);
        p += 4;
        {}
    }}
}}

function exOp(){{
    switch (op & {}) {{
"#,
            self.variables_js(),
            self.read_operations_js(),
            with_n_1_bits(op_size)
        );
        self.functions.iter().enumerate().fold(start, |s, (i, f)| {
            s + &format!("case {}:{}break;", i, f.js())
        }) + "}
            }"
    }

    fn read_operations_js(&self) -> String {
        let mut s = String::new();
        let size = function_discriminant_size_bits(self.functions.len() as u32);
        if size == 0 {
            s += "if(exOp()) return;";
        } else {
            let reads_per_u32 = (32 + (size - 1)) / size;

            for _ in 0..reads_per_u32 {
                s += &format!("if(exOp()) return; op >>>= {};", size);
            }
        }
        s
    }

    fn variables_js(&self) -> String {
        let variables: HashSet<String> = self
            .functions
            .iter()
            .flat_map(|f| f.args.iter().map(|(a, _)| a.to_string()))
            .collect();

        let variables: Vec<_> = variables.into_iter().collect();
        variables.join(",")
    }

    fn to_tokens(&self) -> TokenStream2 {
        let all_js = self.js();
        quote! {
            // force the data to be packed so that we only need to send one pointer to js
            static mut DATA: [u8; 1 + 4 + 4 + 4] = [0; 1 + 4 + 4 + 4];
            static mut METADATA: *mut u8 = unsafe { DATA.as_mut_ptr() };
            static mut DATA_PTR: *mut u32 = unsafe { DATA.as_mut_ptr().add(1).cast() };
            static mut STR_PTR: *mut u32 = unsafe { DATA.as_mut_ptr().add(5).cast() };
            static mut STR_LEN_PTR: *mut u32 = unsafe { DATA.as_mut_ptr().add(9).cast() };
            #[wasm_bindgen(inline_js = #all_js)]
            extern "C" {
                fn run();
                fn update_memory(memory: js_sys::WebAssembly::Memory);
            }
        }
    }
}

#[derive(Debug)]
struct FunctionBinding {
    name: Ident,
    args: Vec<(Ident, SupportedTypes)>,
    body: String,
}

impl FunctionBinding {
    fn js(&self) -> String {
        let (bins, remaining) = pack(&self.args.as_slice(), 32);

        if !remaining.is_empty() {
            todo!();
        }

        let mut s = String::new();

        for b in bins {
            s += "i = m.getUint32(p);";
            s += &b.js();
        }

        s += &self.body;

        s
    }
}

impl Parse for FunctionBinding {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        let function = syn::ItemFn::parse(input)?;
        let name = function.sig.ident;
        let args = function
            .sig
            .inputs
            .iter()
            .map(|arg| match arg {
                syn::FnArg::Receiver(_) => todo!("self"),
                syn::FnArg::Typed(ty) => {
                    let ident = if let Pat::Ident(i) = &*ty.pat {
                        i.ident.clone()
                    } else {
                        panic!("only simple idents are supported")
                    };
                    (ident, SupportedTypes::from(&*ty.ty))
                }
            })
            .collect();

        let body = if let &[syn::Stmt::Expr(Expr::Lit(lit))] = &function.block.stmts.as_slice() {
            if let Lit::Str(s) = &lit.lit {
                s.value()
            } else {
                panic!("missing body")
            }
        } else {
            panic!("missing body")
        };

        Ok(Self { name, args, body })
    }
}

#[derive(Debug, Clone)]
enum SupportedTypes {
    Optional(SimpleTypes),
    SimpleTypes(SimpleTypes),
}

impl SupportedTypes {
    fn size(&self) -> Option<usize> {
        match self {
            SupportedTypes::Optional(t) => t.size().map(|i| i + 1),
            SupportedTypes::SimpleTypes(t) => t.size(),
        }
    }

    fn js(&self, pos: usize) -> String {
        match self {
            SupportedTypes::Optional(t) => todo!(),
            SupportedTypes::SimpleTypes(t) => t.js(pos),
        }
    }
}

#[derive(Debug, Clone)]
enum SimpleTypes {
    Number(Number),
    Slice(Slice),
    Str { len_byte_size: u8 },
}

impl SimpleTypes {
    fn size(&self) -> Option<usize> {
        match self {
            SimpleTypes::Number(n) => Some(n.size()),
            SimpleTypes::Slice(_) => None,
            SimpleTypes::Str { len_byte_size } => Some(*len_byte_size as usize),
        }
    }

    fn js(&self, pos: usize) -> String {
        match self {
            SimpleTypes::Number(n) => n.js(pos),
            SimpleTypes::Slice(s) => s.js(pos),
            SimpleTypes::Str { len_byte_size } => {
                format!(
                    "s.substring(sp, sp += {})",
                    select_bits_js("i", pos, *len_byte_size as usize)
                )
            }
        }
    }
}

impl<'a> From<&'a Type> for SupportedTypes {
    fn from(ty: &'a Type) -> Self {
        if let Type::Path(segments) = ty {
            let segments: Vec<_> = segments.path.segments.iter().collect();
            if let &[simple] = segments.as_slice() {
                let as_str = simple.ident.to_string();
                return SupportedTypes::SimpleTypes(SimpleTypes::Number(match as_str.as_str() {
                    "u8" => Number::U8,
                    "u16" => Number::U16,
                    "u20" => Number::U20,
                    "u24" => Number::U24,
                    "u32" => Number::U32,
                    _ => panic!("unsupported type"),
                }));
            }
        } else if let Type::Reference(ty) = ty {
            if let Type::Path(segments) = &*ty.elem {
                let segments: Vec<_> = segments.path.segments.iter().collect();
                if let &[simple] = segments.as_slice() {
                    let as_str = simple.ident.to_string();
                    if as_str == "str" {
                        if let PathArguments::AngleBracketed(gen) = &simple.arguments {
                            let generics: Vec<_> = gen.args.iter().collect();
                            if let &[GenericArgument::Const(syn::Expr::Lit(lit))] =
                                generics.as_slice()
                            {
                                if let syn::Lit::Int(size) = &lit.lit {
                                    return SupportedTypes::SimpleTypes(SimpleTypes::Str {
                                        len_byte_size: size.base10_parse::<u8>().unwrap(),
                                    });
                                }
                            }
                        } else {
                            return SupportedTypes::SimpleTypes(SimpleTypes::Str {
                                len_byte_size: 4,
                            });
                        }
                    }
                }
            }
            if let Type::Slice(slice) = &*ty.elem {
                if let Type::Path(segments) = &*slice.elem {
                    let segments: Vec<_> = segments.path.segments.iter().collect();
                    if let &[simple] = segments.as_slice() {
                        let as_str = simple.ident.to_string();
                        if let PathArguments::AngleBracketed(gen) = &simple.arguments {
                            let generics: Vec<_> = gen.args.iter().collect();
                            if let &[GenericArgument::Const(syn::Expr::Lit(lit))] =
                                generics.as_slice()
                            {
                                if let syn::Lit::Int(size) = &lit.lit {
                                    let size = size.base10_parse::<u8>().unwrap();
                                    return SupportedTypes::SimpleTypes(SimpleTypes::Slice(
                                        match as_str.as_str() {
                                            "u8" => Slice::U8(size),
                                            "u16" => Slice::U16(size),
                                            "u32" => Slice::U32(size),
                                            _ => panic!("unsupported type"),
                                        },
                                    ));
                                }
                            }
                        } else {
                            return SupportedTypes::SimpleTypes(SimpleTypes::Slice(
                                match as_str.as_str() {
                                    "u8" => Slice::U8(4),
                                    "u16" => Slice::U16(4),
                                    "u32" => Slice::U32(4),
                                    _ => panic!("unsupported type"),
                                },
                            ));
                        }
                    }
                }
            }
        }
        panic!("unsupported type")
    }
}

#[derive(Debug, Clone)]
enum Slice {
    U8(u8),
    U16(u8),
    U32(u8),
}

impl Slice {
    fn len_size(&self) -> usize {
        match self {
            Slice::U8(size) | Slice::U16(size) | Slice::U32(size) => *size as usize,
        }
    }

    // the position must be up to date before calling this
    fn js(&self, pos: usize) -> String {
        let read = select_bits_js("i", pos, self.len_size() * 8);
        match self {
            Slice::U8(_) => format!("new Uint8Array(m.buffer, p, {})", read),
            Slice::U16(_) => format!("new Uint16Array(m.buffer, p, {})", read),
            Slice::U32(_) => format!("new Uint32Array(m.buffer, p, {})", read),
        }
    }
}

#[derive(Debug, Clone)]
enum Number {
    U8,
    U16,
    U20,
    U24,
    U32,
}

impl Number {
    fn size(&self) -> usize {
        match self {
            Number::U8 => 8,
            Number::U16 => 16,
            Number::U20 => 20,
            Number::U24 => 24,
            Number::U32 => 32,
        }
    }

    fn js(&self, pos: usize) -> String {
        match self {
            Number::U8 => select_bits_js("i", pos, 8),
            Number::U16 => select_bits_js("i", pos, 16),
            Number::U20 => select_bits_js("i", pos, 20),
            Number::U24 => select_bits_js("i", pos, 24),
            Number::U32 => select_bits_js("i", pos, 32),
        }
    }
}

struct Bin<I: Item> {
    position: usize,
    remaining: usize,
    filled: Vec<BinEntry<I>>,
}

impl<I: Item> Bin<I> {
    fn new(size: usize) -> Self {
        Self {
            position: 0,
            remaining: size,
            filled: Vec::new(),
        }
    }

    fn push(&mut self, item: I) {
        let size = item.size();
        assert!(size <= self.remaining);
        self.filled.push(BinEntry {
            position: self.position,
            item,
        });
        self.remaining -= size;
        self.position += size;
    }
}

impl Bin<(Ident, SupportedTypes)> {
    fn js(&self) -> String {
        let mut js = String::new();

        // move the pointer forward by the number of bytes read
        js += "p += ";
        js += &((self.position + 7) / 8).to_string();
        js += ";";

        let variables: Vec<_> = self
            .filled
            .iter()
            .map(|entry| format!("{}", entry.item.0))
            .collect();
        js += &variables.join(",");
        js += "=";
        let values: Vec<_> = self
            .filled
            .iter()
            .map(|entry| entry.item.1.js(entry.position))
            .collect();
        js += &values.join(",");
        js += ";";

        js
    }
}

struct BinEntry<I: Item> {
    position: usize,
    item: I,
}

trait Item {
    fn sized(&self) -> bool;
    fn size(&self) -> usize;
}

impl Item for (Ident, SupportedTypes) {
    fn sized(&self) -> bool {
        self.1.size().is_some()
    }

    fn size(&self) -> usize {
        self.1.size().expect("unsized item")
    }
}

fn pack<I: Item + Clone>(items: &[I], bin_size: usize) -> (Vec<Bin<I>>, Vec<I>) {
    // pack items into fixed sized bins

    // first get any unsized items
    let mut sized = Vec::with_capacity(items.len());
    let mut unsized_items = Vec::new();
    for item in items {
        if item.sized() {
            sized.push(item.clone());
        } else {
            unsized_items.push(item.clone());
        }
    }

    // now pack the sized items
    let mut bins: Vec<Bin<I>> = Vec::new();

    // sort the items by size smallest to largest
    sized.sort_unstable_by_key(|i| i.size());

    // now pack the items into bins largest to smallest
    for item in sized.into_iter().rev() {
        let item_size = item.size();
        // find the smallest bin that can fit the item
        let mut smallest_bin = None;
        let mut smallest_bin_size = usize::MAX;
        for bin in &mut bins {
            if bin.remaining >= item_size && smallest_bin_size > bin.remaining {
                smallest_bin_size = bin.remaining;
                smallest_bin = Some(bin);
            }
        }

        if let Some(bin) = smallest_bin {
            bin.push(item);
        } else {
            // create a new bin
            let mut bin = Bin::new(bin_size);
            bin.push(item);
            bins.push(bin);
        }
    }

    (bins, unsized_items)
}
