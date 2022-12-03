//! <div align="center">
//!  <h1>sledgehammer bindgen</h1>
//!  </div>
//!  <div align="center">
//!    <!-- Crates version -->
//!    <a href="https://crates.io/crates/sledgehammer_bindgen">
//!      <img src="https://img.shields.io/crates/v/sledgehammer_bindgen.svg?style=flat-square"
//!      alt="Crates.io version" />
//!    </a>
//!    <!-- Downloads -->
//!    <a href="https://crates.io/crates/sledgehammer_bindgen">
//!      <img src="https://img.shields.io/crates/d/sledgehammer_bindgen.svg?style=flat-square"
//!        alt="Download" />
//!    </a>
//!    <!-- docs -->
//!    <a href="https://docs.rs/sledgehammer_bindgen">
//!      <img src="https://img.shields.io/badge/docs-latest-blue.svg?style=flat-square"
//!        alt="docs.rs docs" />
//!    </a>
//!  </div>
//!  
//!  # What is Sledgehammer Bindgen?
//!  Sledgehammer bindgen provides faster rust batched bindings into javascript code.
//!  
//!  # How does this compare to wasm-bindgen:
//!  - wasm-bindgen is a lot more general it allows returning values and passing around a lot more different types of values. For most users wasm-bindgen is a beter choice. Sledgehammer bindgen is specifically that want low-level, fast access to javascript.
//!  
//!  - You can use sledgehammer bindgen with wasm-bindgen. See the docs and examples for more information.
//!  
//!  # Why is it fast?
//!  
//!  ## String decoding
//!  
//!  - Decoding strings are expensive to decode, but the cost doesn't change much with the size of the string. Wasm-bindgen calls TextDecoder.decode for every string. Sledgehammer only calls TextEncoder.decode once per batch.
//!  
//!  - If the string is small, it is faster to decode the string in javascript to avoid the constant overhead of TextDecoder.decode
//!  
//!  - See this benchmark: https://jsbench.me/4vl97c05lb/5
//!  
//!  ## String Caching
//!  
//!  - You can cache strings in javascript to avoid decoding the same string multiple times.
//!  - If the string is static the string will be hashed by pointer instead of by value which is significantly faster.
//!  
//!  ## Byte encoded operations
//!  
//!  - Every operation is encoded as a sequence of bytes packed into an array. Every operation takes 1 byte plus whatever data is required for it.
//!  
//!  - Each operation is encoded in a batch of four as a u32. Getting a number from an array buffer has a high constant cost, but getting a u32 instead of a u8 is not more expensive. Sledgehammer bindgen reads the u32 and then splits it into the 4 individual bytes. It will shuffle and pack the bytes into as few buckets as possible and try to inline reads into the javascript.
//!  
//!  - See this benchmark: https://jsbench.me/csl9lfauwi/2
use std::collections::HashSet;
use std::ops::Deref;

use proc_macro::TokenStream;
use quote::__private::{Span, TokenStream as TokenStream2};
use quote::quote;
use syn::{
    parse::Parse, parse_macro_input, Expr, GenericArgument, Ident, Lit, Pat, PathArguments, Type,
};
use syn::{ForeignItemFn, ItemFn, TypeParamBound};

/// # Generates bindings for batched calls to js functions. The generated code is a Channel struct with methods for each function.
/// **The function calls to the generated methods are queued and only executed when flush is called.**
///
/// Some of the code generated uses the `sledgehammer_utils` crate, so you need to add that crate as a dependency.
///
/// ```rust, ignore
/// #[bindgen]
/// mod js {
///     // initialize is a special function that is called when the js is initialized. It can be used to set up the js environment.
///     // this is the js code that is executed when the js is loaded.
///     const JS: &str = r#"
///         const text = ["hello"];
///
///         export function get(id) {
///             console.log("got", text[id]);
///             return text[id];
///         }
///     "#;
///
///     // extern blocks allow communicating with wasm-bindgen. The javascript linked is the JS constant above.
///     extern "C" {
///         #[wasm_bindgen]
///         fn get(id: u32) -> String;
///     }
///
///     // valid number types are u8, u16, u24, u32. u24 is defined in the ux crate.
///     fn takes_numbers(n1: u8, n2: u16, n3: u24, n4: u32) {
///         // this is the js code that is executed when takes_numbers is called.
///         // dollar signs around the arguments mark that the arguments are safe to inline (they only appear once).
///         // you can escape dollar signs with a backslash.
///         r#"console.log($n1$, $n2$, $n3$, $n4$, "\$");"#
///     }
///
///     // valid string types are &str<u8>, &str<u16>, &str<u32>.
///     // the generic parameter is the type of the length of the string. u32 is the default.
///     fn takes_strings(str1: &str, str2: &str<u8>) {
///         "console.log($str1$, $str2$);"
///     }
///
///     // you can also use the &str<SIZE, cache_name> syntax to cache the string in a js variable.
///     // each cache has a name that can be reused throughout the bindings so that different functions can share the same cache.
///     // the cache has a size of 128 values.
///     // caches on static strings use the pointer to hash the string which is faster than hashing the string itself.
///     fn takes_cachable_strings(str1: &str<u8, cache1>, str2: &'static str<u16, cache2>) {
///         "console.log($str1$, $str2$);"
///     }
///
///     // Writable allows you to pass in any type that implements the Writable trait.
///     // Because all strings are encoded in a sequental buffer, every string needs to be copied to the new buffer.
///     // If you only create a single string from a Arguments<'_> or number, you can use the Writable trait to avoid allocting a string and then copying it.
///     // the generic parameter is the type of the length of the resulting string. u32 is the default.
///     fn takes_writable(writable: impl Writable<u8>) {
///         "console.log($writable$);"
///     }
///
///     // valid types are &[u8], &[u16], &[u32].
///     // the generic parameter is the type of the length of the array. u32 is the default.
///     fn takes_slices(slice1: &[u8], slice2: &[u8<u16>]) {
///         "console.log($slice1$, $slice2$);"
///     }
/// }
///
/// let mut channel1 = Channel::default();
/// let mut channel2 = Channel::default();
/// channel1.takes_strings("hello", "world");
/// channel1.takes_numbers(1, 2, u24::new(3), 4);
/// channel1.takes_cachable_strings("hello", "world");
/// channel1.takes_cachable_strings("hello", "world");
/// channel1.takes_cachable_strings("hello", "world");
/// channel1.takes_writable(format_args!("hello {}", "world"));
/// // append can be used to append the calls from one channel to another.
/// channel2.append(channel1);
/// channel2.takes_slices(&[1, 2, 3], &[4, 5, 6]);
/// // flush executes all the queued calls and clears the queue.
/// channel2.flush();
/// assert_eq!(get(0), "hello");
/// ```
#[proc_macro_attribute]
pub fn bindgen(_: TokenStream, input: TokenStream) -> TokenStream {
    let mut input = parse_macro_input!(input as Bindings);

    input.as_tokens().into()
}

#[derive(Debug)]
struct Bindings {
    functions: Vec<FunctionBinding>,
    foreign_items: Vec<ForeignItemFn>,
    intialize: Option<String>,
}

impl Parse for Bindings {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        let extren_block = syn::ItemMod::parse(input)?;

        let mut functions = Vec::new();
        let mut foreign_items = Vec::new();
        let mut intialize = None;
        for item in extren_block.content.unwrap().1 {
            match item {
                syn::Item::Const(cnst) => {
                    if cnst.ident == "JS" {
                        let body = if let Expr::Lit(lit) = cnst.expr.deref() {
                            if let Lit::Str(s) = &lit.lit {
                                s.value()
                            } else {
                                panic!("missing body")
                            }
                        } else {
                            panic!("missing body")
                        };
                        intialize = Some(body);
                    }
                }
                syn::Item::Fn(f) => {
                    let f = FunctionBinding::new(f);
                    functions.push(f);
                }
                syn::Item::ForeignMod(m) => {
                    for item in m.items {
                        if let syn::ForeignItem::Fn(f) = item {
                            foreign_items.push(f)
                        }
                    }
                }
                _ => panic!("only functions are supported"),
            }
        }

        Ok(Bindings {
            functions,
            foreign_items,
            intialize,
        })
    }
}

fn function_discriminant_size_bits(function_count: u32) -> usize {
    let len = function_count as u32 + 1;
    let bit_size = (32 - len.next_power_of_two().leading_zeros() as usize).saturating_sub(1);
    match bit_size {
        0..=4 => 4,
        5..=8 => 8,
        _ => panic!("too many functions"),
    }
}

fn with_n_1_bits(n: usize) -> u32 {
    (1u64 << n as u64).saturating_sub(1) as u32
}

fn read_size(size: usize, s: &mut String) {
    match size {
        3..=4 => s.push_str("m.getUint32(p,true)"),
        2 => s.push_str("m.getUint16(p,true)"),
        1..=1 => s.push_str("m.getUint8(p,true)"),
        _ => panic!("invalid size"),
    }
}

struct Read {
    size: usize,
    pos: usize,
}

impl Read {
    fn new(s: &mut String, mut len: usize) -> Self {
        s.push_str("i=");
        read_size(len, s);
        s.push(';');
        // you can't read 3 bytes at once round up to 4
        if len == 3 {
            len = 4;
        }
        Self { size: len, pos: 0 }
    }
}

fn select_bits_js(read: &Read, len: usize) -> String {
    let size = read.size * 8;
    let pos = read.pos * 8;
    select_bits_js_inner("i", size, pos, len)
}

fn select_bits_js_inner(from: &str, size: usize, pos: usize, len: usize) -> String {
    if len == size {
        assert!(pos == 0);
    }
    assert!(len <= size);
    let mut s = String::new();

    if pos != 0 {
        s += &format!("{}>>>{}", from, pos);
    } else {
        s += from;
    }

    if pos + len < size {
        if pos == 0 {
            s += &format!("&{}", with_n_1_bits(len));
        } else {
            s = format!("({})&{}", s, with_n_1_bits(len));
        }
    }

    s
}

impl Bindings {
    fn js(&mut self) -> String {
        let op_size = function_discriminant_size_bits(self.functions.len() as u32);
        let initialize = self.intialize.as_deref().unwrap_or_default();
        let cache_names: HashSet<&Ident> = self
            .functions
            .iter()
            .flat_map(|f| {
                f.args.iter().filter_map(|(_, ty)| match ty {
                    SupportedTypes::SimpleTypes(SimpleTypes::Str(s)) => s.cache_name.as_ref(),
                    _ => None,
                })
            })
            .collect();
        let init_caches = cache_names
            .iter()
            .map(|name| format!("const {} = [];", name))
            .collect::<String>();

        let size = function_discriminant_size_bits(self.functions.len() as u32);
        assert!(size <= 8);
        let reads_per_u32 = (32 + (size - 1)) / size;

        let start = format!(
            r#"let m,p,ls,lss,sp,d,t,c,s,sl,op,i,e,z,{};{}{}export function create(r){{d=r;c=new TextDecoder('utf-8',{{fatal:true}})}}export function update_memory(r){{m=new DataView(r.buffer)}}export function set_buffer(b){{m=new DataView(b)}}export function run(){{t=m.getUint8(d,true);if(t&1){{ls=m.getUint32(d+1,true)}}p=ls;if(t&2){{lss=m.getUint32(d+5,true)}}if(t&4){{sl=m.getUint32(d+9,true);if(t&8){{sp=lss;s="";e=sp+(sl/4|0)*4;while(sp<e){{t=m.getUint32(sp,true);s+=String.fromCharCode(t&255,(t&65280)>>8,(t&16711680)>>16,t>>24);sp+=4}}while(sp<lss+sl){{s+=String.fromCharCode(m.getUint8(sp++));}}}}else{{s=c.decode(new DataView(m.buffer,lss,sl))}}sp=0}}for(;;){{op=m.getUint32(p,true);p+=4;z=0;while(z++<{}){{switch(op&{}){{{}}}}}"#,
            self.variables_js(),
            init_caches,
            initialize,
            reads_per_u32,
            with_n_1_bits(op_size),
            self.functions
                .iter_mut()
                .enumerate()
                .fold(String::new(), |s, (i, f)| {
                    s + &format!("case {}:{}break;", i, f.js())
                })
                + &format!(
                    "case {}:return true;}}op>>>={};}}",
                    self.functions.len(),
                    op_size
                ),
        );
        start
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

    fn as_tokens(&mut self) -> TokenStream2 {
        let all_js = self.js();
        let channel = self.channel();
        let foreign_items = &self.foreign_items;

        let cache_names: HashSet<(&Ident, bool)> = self
            .functions
            .iter()
            .flat_map(|f| {
                f.args.iter().filter_map(|(_, ty)| match ty {
                    SupportedTypes::SimpleTypes(SimpleTypes::Str(s)) => {
                        s.cache_name.as_ref().map(|n| (n, s.static_str))
                    }
                    _ => None,
                })
            })
            .collect();
        let setup = cache_names.iter()
            .map(|(cache, static_str)| {
                if *static_str {
                    quote!{
                        #[allow(non_upper_case_globals)]
                        static mut #cache: sledgehammer_utils::ConstLru<*const str, NonHashBuilder, 128, 256> = sledgehammer_utils::ConstLru::new(NonHashBuilder);
                    }
                }
                else{
                    quote!{
                        #[allow(non_upper_case_globals)]
                        static mut #cache: sledgehammer_utils::once_cell::sync::Lazy<
                            sledgehammer_utils::lru::LruCache<String, u8, std::hash::BuildHasherDefault<sledgehammer_utils::rustc_hash::FxHasher>>,
                        > = sledgehammer_utils::once_cell::sync::Lazy::new(|| {
                            let build_hasher = std::hash::BuildHasherDefault::<sledgehammer_utils::rustc_hash::FxHasher>::default();
                            sledgehammer_utils::lru::LruCache::with_hasher(std::num::NonZeroUsize::new(128).unwrap(), build_hasher)
                        });
                    }
                }
            });
        quote! {
            struct NonHashBuilder;
            impl std::hash::BuildHasher for NonHashBuilder {
                type Hasher = NonHash;
                fn build_hasher(&self) -> Self::Hasher {
                    NonHash(0)
                }
            }
            #[allow(unused)]
            #[derive(Default)]
            struct NonHash(u64);
            impl std::hash::Hasher for NonHash {
                fn finish(&self) -> u64 {
                    self.0
                }
                fn write(&mut self, bytes: &[u8]) {
                    unreachable!()
                }
                fn write_usize(&mut self, i: usize) {
                    self.0 = i as u64;
                }
            }
            // force the data to be packed so that we only need to send one pointer to js
            #[used]
            static mut DATA: [u8; 1 + 4 + 4 + 4] = [255, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0];
            #[used]
            static mut METADATA: *mut u8 = unsafe { DATA.as_mut_ptr() };
            #[used]
            static mut DATA_PTR: *mut *const u8 = unsafe { DATA.as_mut_ptr().add(1).cast() };
            #[used]
            static mut STR_PTR: *mut *const u8 = unsafe { DATA.as_mut_ptr().add(5).cast() };
            #[used]
            static mut STR_LEN_PTR: *mut u32 = unsafe { DATA.as_mut_ptr().add(9).cast() };
            #[used]
            static mut LAST_MEM_SIZE: usize = 0;
            #[wasm_bindgen::prelude::wasm_bindgen(inline_js = #all_js)]
            extern "C" {
                fn create(metadata_ptr: usize);
                fn run();
                fn update_memory(memory: wasm_bindgen::JsValue);
                #(#foreign_items)*
            }
            #(#setup)*
            #channel
            const GENERATED_JS: &str = #all_js;
        }
    }

    fn channel(&self) -> TokenStream2 {
        let methods = self
            .functions
            .iter()
            .enumerate()
            .map(|(i, f)| f.to_tokens(i as u8));
        let end_msg = self.functions.len() as u8;
        let no_op = self.functions.len() as u8 + 1;
        let size = function_discriminant_size_bits(self.functions.len() as u32);
        let reads_per_u32 = (32 + (size - 1)) / size;
        let encode_op = match size {
            4 => {
                quote! {
                    if self.current_op_byte_idx % 2 == 0 {
                        *self.msg.as_mut_ptr()
                            .add(self.current_op_batch_idx + self.current_op_byte_idx / 2) = op;
                    } else {
                        *self.msg.as_mut_ptr()
                            .add(self.current_op_batch_idx + self.current_op_byte_idx / 2) |= op << 4;
                    }
                    self.current_op_byte_idx += 1;
                }
            }
            8 => {
                quote! {
                    *self.msg.as_mut_ptr()
                            .add(self.current_op_batch_idx + self.current_op_byte_idx) = op;
                    self.current_op_byte_idx += 1;
                }
            }
            _ => panic!("unsupported size"),
        };
        quote! {
            pub struct Channel {
                msg: Vec<u8>,
                str_buffer: Vec<u8>,
                current_op_batch_idx: usize,
                current_op_byte_idx: usize,
            }

            impl Default for Channel {
                fn default() -> Self {
                    Self {
                        msg: Vec::new(),
                        str_buffer: Vec::new(),
                        current_op_batch_idx: 0,
                        current_op_byte_idx: #reads_per_u32,
                    }
                }
            }

            impl Channel {
                pub fn append(&mut self, mut batch: Self) {
                    // add empty operations to the batch to make sure the batch is aligned
                    let operations_left = #reads_per_u32 - self.current_op_byte_idx;
                    for _ in 0..operations_left {
                        self.encode_op(#no_op);
                    }

                    self.current_op_byte_idx = batch.current_op_byte_idx;
                    self.current_op_batch_idx = self.msg.len() + batch.current_op_batch_idx;
                    self.str_buffer.extend_from_slice(&batch.str_buffer);
                    self.msg.append(&mut batch.msg);
                }

                #[allow(clippy::uninit_vec)]
                fn encode_op(&mut self, op: u8) {
                    unsafe{
                        if self.current_op_byte_idx >= #reads_per_u32 {
                            self.current_op_batch_idx = self.msg.len();
                            self.msg.reserve(4);
                            self.msg.set_len(self.msg.len() + 4);
                            self.current_op_byte_idx = 0;
                        }
                        #encode_op
                    }
                }

                pub fn flush(&mut self){
                    #[cfg(target_family = "wasm")]
                    {
                        self.encode_op(#end_msg);
                        let msg_ptr = self.msg.as_ptr();
                        let str_ptr = self.str_buffer.as_ptr();
                        // the pointer will only be updated when the message vec is resized, so we have a flag to check if the pointer has changed to avoid unnecessary decoding
                        if unsafe { *METADATA } == 255 {
                            // this is the first message, so we need to encode all the metadata
                            unsafe {
                                create(DATA.as_mut_ptr() as usize);
                                *DATA_PTR = msg_ptr;
                                *STR_PTR = str_ptr;
                                *METADATA = 7;
                            }
                        } else {
                            if unsafe { *DATA_PTR } != msg_ptr {
                                unsafe {
                                    *DATA_PTR = msg_ptr;
                                    // the first bit encodes if the msg pointer has changed
                                    *METADATA = 1;
                                }
                            } else {
                                unsafe {
                                    // the first bit encodes if the msg pointer has changed
                                    *METADATA = 0;
                                }
                            }
                            if unsafe { *STR_PTR } != str_ptr {
                                unsafe {
                                    *STR_PTR = str_ptr;
                                    // the second bit encodes if the str pointer has changed
                                    *METADATA |= 2;
                                }
                            }
                        }
                        unsafe {
                            if !self.str_buffer.is_empty() {
                                // the third bit encodes if there is any strings
                                *METADATA |= 4;
                                *STR_LEN_PTR = self.str_buffer.len() as u32;
                                if *STR_LEN_PTR < 100 {
                                    // the fourth bit encodes if the strings are entirely ascii and small
                                    *METADATA |= (self.str_buffer.is_ascii() as u8) << 3;
                                }
                            }
                        }
                        let new_mem_size = core::arch::wasm32::memory_size(0);
                        unsafe{
                            // we need to update the memory if the memory has grown
                            if new_mem_size != LAST_MEM_SIZE {
                                LAST_MEM_SIZE = new_mem_size;
                                update_memory(wasm_bindgen::memory());
                            }
                        }

                        run();
                        self.current_op_batch_idx = 0;
                        self.current_op_byte_idx = #reads_per_u32;
                        self.str_buffer.clear();
                        self.msg.clear();
                    }
                }

                #(#methods)*
            }
        }
    }
}

#[derive(Debug)]
struct JsBody {
    segments: Vec<(String, String)>,
    trailing: String,
}

fn parse_js_body(s: &str) -> JsBody {
    let mut inside_param = false;
    let mut last_was_escape = false;
    let mut current_param = String::new();
    let mut current_segment = String::new();
    let mut segments = Vec::new();
    for c in s.chars() {
        match c {
            '\\' => last_was_escape = true,
            '$' => {
                if !last_was_escape {
                    if inside_param {
                        segments.push((current_segment, current_param));
                        current_segment = String::new();
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
                    current_segment.push(c);
                }
            }
        }
    }
    JsBody {
        segments,
        trailing: current_segment,
    }
}

#[test]
fn parse_body() {
    let js = "console.log($i$, $y$);";
    let body = parse_js_body(js);
    assert_eq!(
        body.segments,
        [
            ("console.log(".to_string(), "i".to_string()),
            (", ".to_string(), "y".to_string())
        ]
    );
    assert_eq!(body.trailing, ");".to_string());
}

#[derive(Debug)]
struct FunctionBinding {
    name: Ident,
    args: Vec<(Ident, SupportedTypes)>,
    body: JsBody,
    bins: Vec<Bin<(Ident, SupportedTypes)>>,
    remaining: Vec<(Ident, SupportedTypes)>,
}

impl FunctionBinding {
    fn new(function: ItemFn) -> Self {
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
        let body = parse_js_body(&body);

        Self {
            name,
            args,
            body,
            bins: Vec::new(),
            remaining: Vec::new(),
        }
    }

    fn js(&mut self) -> String {
        let args = self.args.clone();
        let (bins, remaining) = pack(args, 4);

        self.bins = bins;
        // move the bin with the most inlinable variables to the end
        let mut most_inlinable_idx = None;
        let mut most_inlinable_count = 0;
        for (i, bin) in self.bins.iter().enumerate() {
            let inlinable_count = bin
                .filled
                .iter()
                .map(|entry| {
                    if entry.item.1.inlinable() {
                        self.body
                            .segments
                            .iter()
                            .filter(|(_, param)| entry.item.0 == param)
                            .count()
                    } else {
                        0
                    }
                })
                .sum();
            if most_inlinable_idx.is_none() || inlinable_count > most_inlinable_count {
                most_inlinable_count = inlinable_count;
                most_inlinable_idx = Some(i);
            }
        }
        if let Some(idx) = most_inlinable_idx {
            let len = self.bins.len();
            self.bins.swap(idx, len - 1);
        }
        self.remaining = remaining;

        let mut s = String::new();
        for (name, ty) in &self.remaining {
            s += &ty.js_inlined(name.to_string());
        }
        for b in &self.bins[..self.bins.len().saturating_sub(1)] {
            s += &b.js();
        }

        // we can inline variables from the last bin
        if let Some(last_bin) = self.bins.last_mut() {
            if let &[BinEntry {
                item: (name, SupportedTypes::SimpleTypes(ty)),
            }] = &last_bin.filled.as_slice()
            {
                // the read can also be inlined
                if self.body.segments.iter().any(|(_, param)| name == param) {
                    if let Some(get) = ty.js_get_inlined() {
                        for (segment, param) in &self.body.segments {
                            s += segment;
                            if name == param {
                                s += &get;
                            } else {
                                s += param;
                            }
                        }
                        s += &self.body.trailing;
                        s += "p+=";
                        s += &ty.min_size().to_string();
                        s += ";";
                        return s;
                    }
                }
            }

            // we need to shuffle the data so that the strings are read in the same order as they are written
            let mut new_last_bin_filled = Vec::new();

            let mut read = Read::new(&mut s, last_bin.position);
            // first insert any non inlinable parameters
            let mut i = 0;
            'o: while i < last_bin.filled.len() {
                let entry = &last_bin.filled[i];
                let (name, ty) = &entry.item;
                let mut matched = false;
                for (_, param) in &self.body.segments {
                    if name == param {
                        if !ty.inlinable() {
                            s += &ty.js(name.to_string(), &mut read);
                            new_last_bin_filled.push(last_bin.filled.remove(i));
                            continue 'o;
                        }
                        matched = true;
                    }
                }
                if !matched {
                    s += &ty.js(name.to_string(), &mut read);
                    new_last_bin_filled.push(last_bin.filled.remove(i));
                    continue;
                }
                i += 1;
            }

            // fill in the remaining inlinable parameters
            for (segment, param) in &self.body.segments {
                s += segment;
                let mut inlined = false;
                let mut i = 0;
                while i < last_bin.filled.len() {
                    let entry = &last_bin.filled[i];
                    let (name, ty) = &entry.item;
                    if name == param {
                        if let Some(get) = ty.js_get(&mut read) {
                            s += &get;
                            inlined = true;
                            new_last_bin_filled.push(last_bin.filled.remove(i));
                            break;
                        }
                    }
                    i += 1;
                }
                if !inlined {
                    s += param;
                }
            }
            s += &self.body.trailing;
            s += "p+=";
            s += &last_bin.position.to_string();
            s += ";";

            last_bin.filled = new_last_bin_filled;
        } else {
            for (segment, param) in &self.body.segments {
                s += segment;
                s += param;
            }
            s += &self.body.trailing;
        }

        s
    }

    fn to_tokens(&self, index: u8) -> TokenStream2 {
        let name = &self.name;
        let args: Vec<_> = self.args.iter().map(|(a, _)| a).collect();
        let types = self.args.iter().map(|(_, t)| t.to_tokens());
        let encode_types = self
            .remaining
            .iter()
            .chain(
                self.bins
                    .iter()
                    .flat_map(|bin| bin.filled.iter().map(|entry| &entry.item)),
            )
            .map(|(i, t)| t.encode(i));
        let size: usize = self.args.iter().map(|(_, t)| t.min_size()).sum();
        let reserve = if size == 0 {
            quote! {}
        } else {
            quote! {self.msg.reserve(#size);}
        };
        quote! {
            #[allow(clippy::uninit_vec)]
            pub fn #name(&mut self, #(#args: #types),*) {
                self.encode_op(#index);
                #reserve
                unsafe{
                    #(
                        #encode_types;
                    )*
                }
            }
        }
    }
}

#[derive(Debug, Clone)]
#[allow(unused)]
enum SupportedTypes {
    Optional(SimpleTypes),
    SimpleTypes(SimpleTypes),
}

impl SupportedTypes {
    fn read_from_u32(&self) -> bool {
        match self {
            SupportedTypes::Optional(t) => t.read_from_u32(),
            SupportedTypes::SimpleTypes(t) => t.read_from_u32(),
        }
    }

    fn inlinable(&self) -> bool {
        match self {
            SupportedTypes::Optional(t) => t.inlinable(),
            SupportedTypes::SimpleTypes(t) => t.inlinable(),
        }
    }

    fn min_size(&self) -> usize {
        match self {
            SupportedTypes::Optional(t) => t.min_size() + 1,
            SupportedTypes::SimpleTypes(t) => t.min_size(),
        }
    }

    fn js(&self, parameter: String, read: &mut Read) -> String {
        match self {
            SupportedTypes::Optional(_) => todo!(),
            SupportedTypes::SimpleTypes(t) => t.js(parameter, read),
        }
    }

    fn js_inlined(&self, parameter: String) -> String {
        match self {
            SupportedTypes::Optional(_) => todo!(),
            SupportedTypes::SimpleTypes(t) => t.js_inlined(parameter),
        }
    }

    fn js_get(&self, read: &mut Read) -> Option<String> {
        match self {
            SupportedTypes::Optional(_) => todo!(),
            SupportedTypes::SimpleTypes(t) => t.js_get(read),
        }
    }

    fn to_tokens(&self) -> TokenStream2 {
        match self {
            SupportedTypes::Optional(t) => {
                let t = t.to_tokens();
                quote! {
                    Option<#t>
                }
            }
            SupportedTypes::SimpleTypes(t) => t.to_tokens(),
        }
    }

    fn encode(&self, name: &Ident) -> TokenStream2 {
        match self {
            SupportedTypes::Optional(_) => {
                todo!()
            }
            SupportedTypes::SimpleTypes(t) => t.encode(name),
        }
    }
}

#[derive(Debug, Clone)]
enum SimpleTypes {
    Number(Number),
    Slice(Slice),
    Str(Str),
    Writable(Writable),
}

impl SimpleTypes {
    fn read_from_u32(&self) -> bool {
        match self {
            SimpleTypes::Number(_) => true,
            SimpleTypes::Slice(_) => false,
            SimpleTypes::Str(_) => true,
            SimpleTypes::Writable(_) => true,
        }
    }

    fn inlinable(&self) -> bool {
        match self {
            SimpleTypes::Number(_) => true,
            SimpleTypes::Slice(_) => false,
            SimpleTypes::Str(s) => s.inlinable(),
            SimpleTypes::Writable(_) => true,
        }
    }

    fn min_size(&self) -> usize {
        match self {
            SimpleTypes::Number(n) => n.size(),
            SimpleTypes::Slice(s) => s.min_size(),
            SimpleTypes::Str(s) => s.min_size(),
            SimpleTypes::Writable(w) => w.min_size(),
        }
    }

    fn js(&self, parameter: String, read: &mut Read) -> String {
        match self {
            SimpleTypes::Number(n) => n.js(parameter, read),
            SimpleTypes::Slice(_) => panic!("not supported"),
            SimpleTypes::Str(s) => s.js(parameter, read),
            SimpleTypes::Writable(w) => w.js(parameter, read),
        }
    }

    fn js_inlined(&self, parameter: String) -> String {
        match self {
            SimpleTypes::Number(n) => n.js_inlined(parameter),
            SimpleTypes::Slice(s) => s.js_inlined(parameter),
            SimpleTypes::Str(s) => s.js_inlined(parameter),
            SimpleTypes::Writable(w) => w.js_inlined(parameter),
        }
    }

    fn js_get_inlined(&self) -> Option<String> {
        match self {
            SimpleTypes::Number(n) => Some(n.js_get_inlined()),
            SimpleTypes::Slice(_) => None,
            SimpleTypes::Str(s) => s.js_get_inlined(),
            SimpleTypes::Writable(w) => Some(w.js_get_inlined()),
        }
    }

    fn js_get(&self, read: &mut Read) -> Option<String> {
        match self {
            SimpleTypes::Number(n) => Some(n.js_get(read)),
            SimpleTypes::Slice(_) => None,
            SimpleTypes::Str(s) => s.js_get(read),
            SimpleTypes::Writable(w) => Some(w.js_get(read)),
        }
    }

    fn to_tokens(&self) -> TokenStream2 {
        match self {
            SimpleTypes::Number(n) => n.to_tokens(),
            SimpleTypes::Slice(s) => s.to_tokens(),
            SimpleTypes::Str(s) => s.to_tokens(),
            SimpleTypes::Writable(w) => w.to_tokens(),
        }
    }

    fn encode(&self, name: &Ident) -> TokenStream2 {
        match self {
            SimpleTypes::Number(n) => n.encode(name),
            SimpleTypes::Slice(s) => s.encode(name),
            SimpleTypes::Str(s) => s.encode(name),
            SimpleTypes::Writable(w) => w.encode(name),
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
                            if let GenericArgument::Type(Type::Path(t)) = &generics[0] {
                                let segments: Vec<_> = t.path.segments.iter().collect();
                                if let &[simple] = segments.as_slice() {
                                    let mut cache = None;
                                    if let Some(GenericArgument::Type(Type::Path(t))) =
                                        &generics.get(1)
                                    {
                                        let segments: Vec<_> = t.path.segments.iter().collect();
                                        if let &[simple] = segments.as_slice() {
                                            cache = Some(simple.ident.clone());
                                        }
                                    }
                                    return SupportedTypes::SimpleTypes(SimpleTypes::Str(Str {
                                        size_type: match simple.ident.to_string().as_str() {
                                            "u8" => Number::U8,
                                            "u16" => Number::U16,
                                            "u24" => Number::U24,
                                            "u32" => Number::U32,
                                            _ => panic!("unsupported type"),
                                        },
                                        cache_name: cache,
                                        static_str: ty
                                            .lifetime
                                            .as_ref()
                                            .filter(|l| l.ident == "static")
                                            .is_some(),
                                    }));
                                }
                            }
                        }
                        return SupportedTypes::SimpleTypes(SimpleTypes::Str(Str {
                            size_type: Number::U32,
                            cache_name: None,
                            static_str: ty
                                .lifetime
                                .as_ref()
                                .filter(|l| l.ident == "static")
                                .is_some(),
                        }));
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
                            if let &[GenericArgument::Type(Type::Path(t))] = generics.as_slice() {
                                let segments: Vec<_> = t.path.segments.iter().collect();
                                if let &[simple] = segments.as_slice() {
                                    let size = match simple.ident.to_string().as_str() {
                                        "u8" => Number::U8,
                                        "u16" => Number::U16,
                                        "u32" => Number::U32,
                                        _ => panic!("unsupported type"),
                                    };
                                    return SupportedTypes::SimpleTypes(SimpleTypes::Slice(
                                        Slice {
                                            size,
                                            inner: match as_str.as_str() {
                                                "u8" => Number::U8,
                                                "u16" => Number::U16,
                                                "u32" => Number::U32,
                                                _ => panic!("unsupported type"),
                                            },
                                        },
                                    ));
                                }
                            }
                        } else {
                            return SupportedTypes::SimpleTypes(SimpleTypes::Slice(Slice {
                                size: Number::U32,
                                inner: match as_str.as_str() {
                                    "u8" => Number::U8,
                                    "u16" => Number::U16,
                                    "u32" => Number::U32,
                                    _ => panic!("unsupported type"),
                                },
                            }));
                        }
                    }
                }
            }
        } else if let Type::ImplTrait(tr) = ty {
            let traits: Vec<_> = tr.bounds.iter().collect();
            if let &[TypeParamBound::Trait(tr)] = traits.as_slice() {
                let segments: Vec<_> = tr.path.segments.iter().collect();
                if let &[simple] = segments.as_slice() {
                    if simple.ident == "Writable" {
                        if let PathArguments::AngleBracketed(gen) = &simple.arguments {
                            let generics: Vec<_> = gen.args.iter().collect();
                            if let &[GenericArgument::Type(Type::Path(t))] = generics.as_slice() {
                                let segments: Vec<_> = t.path.segments.iter().collect();
                                if let &[simple] = segments.as_slice() {
                                    let size = match simple.ident.to_string().as_str() {
                                        "u8" => Number::U8,
                                        "u16" => Number::U16,
                                        "u32" => Number::U32,
                                        _ => panic!("unsupported type"),
                                    };
                                    return SupportedTypes::SimpleTypes(SimpleTypes::Writable(
                                        Writable { size_type: size },
                                    ));
                                }
                            }
                        } else {
                            return SupportedTypes::SimpleTypes(SimpleTypes::Writable(Writable {
                                size_type: Number::U32,
                            }));
                        }
                    }
                }
            }
        }
        panic!("unsupported type")
    }
}

#[derive(Debug, Clone)]
struct Slice {
    size: Number,
    inner: Number,
}

impl Slice {
    fn min_size(&self) -> usize {
        self.size.size() + 4
    }

    fn js_inlined(&self, parameter: String) -> String {
        let ptr_read = Number::U32.js_get_inlined();
        let len_read = match self.size {
            Number::U8 => "m.getUint8(p+=4,true)".to_string(),
            Number::U16 => "m.getUint16(p+=4,true)".to_string(),
            Number::U32 => "m.getUint32(p+=4,true)".to_string(),
            _ => panic!("unsupported length type"),
        };
        let read = match self.inner {
            Number::U8 => format!("new Uint8Array(m.buffer,{},{});", ptr_read, len_read),
            Number::U16 => format!("new Uint16Array(m.buffer,{},{});", ptr_read, len_read),
            Number::U32 => format!("new Uint32Array(m.buffer,{},{});", ptr_read, len_read),
            _ => todo!(),
        };
        parameter + "=" + &read + "p+=" + &self.size.size().to_string() + ";"
    }

    fn to_tokens(&self) -> TokenStream2 {
        let inner = self.inner.to_tokens();
        quote! { &[#inner] }
    }

    fn encode(&self, name: &Ident) -> TokenStream2 {
        let len = Ident::new("len", Span::call_site());
        let encode_len = self.size.encode(&len);
        let ptr = Ident::new("ptr", Span::call_site());
        let encode_ptr = Number::U32.encode(&ptr);
        quote! {
            let #ptr = #name.as_ptr();
            #encode_ptr
            let #len = #name.len();
            #encode_len
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
enum Number {
    U8,
    U16,
    U24,
    U32,
}

impl Number {
    fn size(&self) -> usize {
        match self {
            Number::U8 => 1,
            Number::U16 => 2,
            Number::U24 => 3,
            Number::U32 => 4,
        }
    }

    fn js(&self, parameter: String, read: &mut Read) -> String {
        let r = format!("{}={};", parameter, self.js_get(read));
        read.pos += self.size();
        r
    }

    fn js_inlined(&self, parameter: String) -> String {
        format!("{}={};", parameter, self.js_get_inlined())
    }

    fn js_get_inlined(&self) -> String {
        let mut read = String::new();
        read_size(self.size(), &mut read);
        match self {
            Number::U8 => select_bits_js_inner(&read, 8, 0, 8),
            Number::U16 => select_bits_js_inner(&read, 16, 0, 16),
            Number::U24 => select_bits_js_inner(&read, 32, 0, 24),
            Number::U32 => select_bits_js_inner(&read, 32, 0, 32),
        }
    }

    fn js_get(&self, read: &mut Read) -> String {
        let r = match self {
            Number::U8 => select_bits_js(read, 8),
            Number::U16 => select_bits_js(read, 16),
            Number::U24 => select_bits_js(read, 24),
            Number::U32 => select_bits_js(read, 32),
        };
        read.pos += self.size();
        r
    }

    fn to_tokens(&self) -> TokenStream2 {
        match self {
            Number::U8 => quote! { u8 },
            Number::U16 => quote! { u16 },
            Number::U24 => quote! { u24 },
            Number::U32 => quote! { u32 },
        }
    }

    fn encode(&self, name: &Ident) -> TokenStream2 {
        let len_byte_size = self.size() as usize;
        let encode_write = self.encode_write(name);
        quote! {
            #encode_write
            self.msg.set_len(self.msg.len() + #len_byte_size);
        }
    }

    fn encode_write(&self, name: &Ident) -> TokenStream2 {
        let ty = self.to_tokens();
        if let Number::U24 = self {
            quote! {
                let as_u32 = u32::from(#name);
                *self.msg.as_mut_ptr().add(self.msg.len()).cast() = as_u32;
            }
        } else {
            quote! {
                *self.msg.as_mut_ptr().add(self.msg.len()).cast() = #name as #ty;
            }
        }
    }
}

#[derive(Clone, Debug)]
struct Str {
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

    fn js_get(&self, read: &mut Read) -> Option<String> {
        self.cache_name
            .is_none()
            .then(|| format!("s.substring(sp,sp+={})", self.size_type.js_get(read)))
    }

    fn js_inlined(&self, parameter: String) -> String {
        format!("{}={};", parameter, self.js_get_inlined().unwrap())
    }

    fn js_get_inlined(&self) -> Option<String> {
        self.cache_name
            .is_none()
            .then(|| format!("s.substring(sp,sp+={})", self.size_type.js_get_inlined()))
    }

    fn encode(&self, name: &Ident) -> TokenStream2 {
        let len = Ident::new("len", Span::call_site());
        let write_len = self.size_type.encode(&len);
        let len_byte_size = self.size_type.size();
        let encode = quote! {
            unsafe {
                let #len = #name.len();
                #write_len
                let old_len = self.str_buffer.len();
                self.str_buffer.reserve(#len);
                let ptr = self.str_buffer.as_mut_ptr().add(old_len);
                let bytes = #name.as_bytes();
                let str_ptr = bytes.as_ptr();
                for o in 0..#len {
                    *ptr.add(o) = *str_ptr.add(o);
                }
                self.str_buffer.set_len(old_len + #len);
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
                            unsafe{
                                self.msg.set_len(self.msg.len() + #len_byte_size + 1);
                            }
                        }
                    }
                } else {
                    quote! {
                        if let Some(&id) = #cache.get(#name){
                            *self.msg.as_mut_ptr().add(self.msg.len()) = id;
                            unsafe{
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

#[derive(Clone, Debug)]
struct Writable {
    size_type: Number,
}

impl Writable {
    fn to_tokens(&self) -> TokenStream2 {
        quote! { impl sledgehammer_utils::Writable }
    }

    fn min_size(&self) -> usize {
        self.size_type.size()
    }

    fn js(&self, parameter: String, read: &mut Read) -> String {
        let s = format!(
            "{}=s.substring(sp,sp+={});",
            parameter,
            self.size_type.js_get(read),
        );
        read.pos += self.min_size();
        s
    }

    fn js_inlined(&self, parameter: String) -> String {
        format!(
            "{}=s.substring(sp,sp+={});",
            parameter,
            self.size_type.js_get_inlined(),
        )
    }

    fn js_get(&self, read: &mut Read) -> String {
        format!("s.substring(sp,sp+={})", self.size_type.js_get(read))
    }

    fn js_get_inlined(&self) -> String {
        format!("s.substring(sp,sp+={})", self.size_type.js_get_inlined())
    }

    fn encode(&self, name: &Ident) -> TokenStream2 {
        let len = Ident::new("len", Span::call_site());
        let write_len = self.size_type.encode(&len);
        quote! {
            unsafe {
                let prev_len = self.str_buffer.len();
                #name.write(&mut self.str_buffer);
                // the length of the string is the change in length of the string buffer
                let #len = self.str_buffer.len() - prev_len;
                #write_len
            }
        }
    }
}

#[derive(Debug)]
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
        self.filled.push(BinEntry { item });
        self.remaining -= size;
        self.position += size;
    }
}

impl Bin<(Ident, SupportedTypes)> {
    fn js(&self) -> String {
        if let &[BinEntry {
            item: (name, SupportedTypes::SimpleTypes(ty)),
        }] = &self.filled.as_slice()
        {
            if ty.inlinable() {
                let mut js = ty.js_inlined(name.to_string());
                // move the pointer forward by the number of bytes read
                js += "p += ";
                js += &self.position.to_string();
                js += ";";
                return js;
            }
        }

        let mut js = String::new();

        let mut read = Read::new(&mut js, self.position);

        // move the pointer forward by the number of bytes read
        js += "p += ";
        js += &self.position.to_string();
        js += ";";

        for entry in &self.filled {
            js += &entry.item.1.js(entry.item.0.to_string(), &mut read);
        }

        js
    }
}

#[derive(Debug)]
struct BinEntry<I: Item> {
    item: I,
}

trait Item {
    fn sized(&self) -> bool;
    fn size(&self) -> usize;
}

impl Item for (Ident, SupportedTypes) {
    fn sized(&self) -> bool {
        self.1.read_from_u32()
    }

    fn size(&self) -> usize {
        self.1.min_size()
    }
}

fn pack<I: Item>(items: Vec<I>, bin_size: usize) -> (Vec<Bin<I>>, Vec<I>) {
    // pack items into fixed sized bins

    // first get any unsized items
    let mut sized = Vec::with_capacity(items.len());
    let mut unsized_items = Vec::new();
    for item in items {
        if item.sized() {
            sized.push(item);
        } else {
            unsized_items.push(item);
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
