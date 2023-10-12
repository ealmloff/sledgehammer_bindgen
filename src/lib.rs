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
//!  - See this benchmark: <https://jsbench.me/4vl97c05lb/5>
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
//!  - See this benchmark: <https://jsbench.me/csl9lfauwi/2>
use crate::encoder::Encoder;
use builder::{RustJSFlag, RustJSU32};
use encoder::Encoders;
use function::FunctionBinding;
use proc_macro::TokenStream;
use quote::__private::{Span, TokenStream as TokenStream2};
use quote::quote;
use std::collections::HashSet;
use std::ops::Deref;
use syn::parse::ParseStream;
use syn::punctuated::Punctuated;
use syn::spanned::Spanned;
use syn::{parse::Parse, parse_macro_input, Expr, Ident, Lit};
use syn::{parse_quote, ForeignItemFn};
use types::string::GeneralStringFactory;

mod builder;
mod encoder;
mod function;
mod types;

/// # Generates bindings for batched calls to js functions. The generated code is a Buffer struct with methods for each function.
/// **The function calls to the generated methods are queued and only executed when flush is called.**
///
/// Some of the code generated uses the `sledgehammer_utils` crate, so you need to add that crate as a dependency.
///
/// ```rust, ignore
/// #[bindgen]
/// mod js {
///     // You can define a struct to hold the data for the batched calls.
///     struct Buffer;
///
///     // JS is a special constatant that defines initialization javascript. It can be used to set up the js environment and define the code that wasm-bindgen binds to.
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
///     // valid number types are u8, u16, u32.
///     fn takes_numbers(n1: u8, n2: u16, n3: u32) {
///         // this is the js code that is executed when takes_numbers is called.
///         // dollar signs around the arguments mark that the arguments are safe to inline (they only appear once).
///         // you can escape dollar signs with a backslash.
///         r#"console.log($n1$, $n2$, $n3$, "\$");"#
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
/// let mut channel1 = Buffer::default();
/// let mut channel2 = Buffer::default();
/// channel1.takes_strings("hello", "world");
/// channel1.takes_numbers(1, 2, 3);
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
pub fn bindgen(args: TokenStream, input: TokenStream) -> TokenStream {
    let args = parse_macro_input!(args as Args);

    let mut input = parse_macro_input!(input as Bindings);
    input.args = args;

    input.as_tokens().into()
}

struct Args {
    module: bool,
}

impl Parse for Args {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let arguments: Punctuated<Ident, syn::Token![,]> = Punctuated::parse_terminated(input)?;
        let mut module = false;
        for arg in arguments {
            match arg.to_string().as_str() {
                "module" => module = true,
                _ => panic!("unknown argument"),
            }
        }

        Ok(Args { module })
    }
}

struct Bindings {
    args: Args,
    buffer: Ident,
    functions: Vec<FunctionBinding>,
    foreign_items: Vec<ForeignItemFn>,
    initialize: String,
    encoders: Encoders,
    msg_ptr_u32: RustJSU32,
    msg_moved_flag: RustJSFlag,
}

impl Parse for Bindings {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        let extren_block = syn::ItemMod::parse(input)?;

        let mut buffer = None;
        let mut functions = Vec::new();
        let mut foreign_items = Vec::new();
        let mut initialize = String::new();
        let mut encoders = Encoders::default();
        encoders.insert(GeneralStringFactory);
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
                        initialize += &body;
                    }
                    if cnst.ident == "JS_FILE" {
                        let path = if let Expr::Lit(lit) = cnst.expr.deref() {
                            if let Lit::Str(s) = &lit.lit {
                                s.value()
                            } else {
                                panic!("missing body")
                            }
                        } else {
                            panic!("missing body")
                        };
                        initialize += &std::fs::read_to_string(&path).map_err(|e| {
                            syn::Error::new(
                                cnst.span(),
                                format!(
                                    "failed to read file {} (from dir {}): {}",
                                    path,
                                    std::env::current_dir().unwrap().display(),
                                    e
                                ),
                            )
                        })?;
                    }
                }
                syn::Item::Fn(f) => {
                    let f = FunctionBinding::new(&mut encoders, f);
                    functions.push(f);
                }
                syn::Item::ForeignMod(m) => {
                    for item in m.items {
                        if let syn::ForeignItem::Fn(f) = item {
                            foreign_items.push(f)
                        }
                    }
                }
                syn::Item::Struct(strct) => {
                    buffer = Some(strct.ident);
                }
                _ => panic!("only functions are supported"),
            }
        }

        for encoder in encoders.values() {
            initialize += &encoder.global_js();
        }

        let msg_ptr_u32 = encoders.builder().u32();
        let msg_moved_flag = encoders.builder().flag();

        Ok(Bindings {
            args: Args { module: false },
            buffer: buffer.unwrap_or(Ident::new("Channel", Span::call_site())),
            functions,
            foreign_items,
            initialize,
            encoders,
            msg_ptr_u32,
            msg_moved_flag,
        })
    }
}

fn function_discriminant_size_bits(function_count: u32) -> usize {
    let len = function_count + 1;
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
        let initialize = &self.initialize;

        let size = function_discriminant_size_bits(self.functions.len() as u32);
        assert!(size <= 8);
        let reads_per_u32 = (32 + (size - 1)) / size;

        let op_mask = with_n_1_bits(op_size);

        let match_op = self
            .functions
            .iter_mut()
            .enumerate()
            .fold(String::new(), |s, (i, f)| {
                s + &format!("case {}:{}break;", i, f.js())
            })
            + &format!("case {}:return true;", self.functions.len(),);

        let pre_run_js = self
            .encoders
            .values()
            .fold(String::new(), |s, e| s + &e.pre_run_js());

        let msg_ptr_moved = self.msg_moved_flag.read_js();
        let read_msg_ptr = self.msg_ptr_u32.read_js();

        let pre_run_metadata = self.encoders.builder().pre_run_js();

        let all_variables: HashSet<&str> = self
            .functions
            .iter()
            .flat_map(|f| f.variables.iter().map(|s| s.as_str()))
            .collect();

        let declarations = if all_variables.is_empty() {
            String::new()
        } else {
            format!(
                "let {};",
                all_variables.into_iter().collect::<Vec<_>>().join(",")
            )
        };

        let maybe_export = if self.args.module { "export " } else { "" };

        let js = format!(
            r#"let m,p,ls,d,t,op,i,e,z,metaflags;
            {initialize}
            {declarations}
            {maybe_export} function create(r){{
                d=r;
            }}
            {maybe_export} function update_memory(b){{
                m=new DataView(b.buffer)
            }}
            {maybe_export} function run(){{
                {pre_run_metadata}
                if({msg_ptr_moved}){{
                    ls={read_msg_ptr};
                }}
                p=ls;
                {pre_run_js}
                for(;;){{
                    op=m.getUint32(p,true);
                    p+=4;
                    z=0;
                    while(z++<{reads_per_u32}){{
                        switch(op&{op_mask}){{
                            {match_op}
                        }}
                        op>>>={op_size};
                    }}
                }}
            }}
            {maybe_export} function run_from_bytes(bytes){{
                d = 0;
                update_memory(new Uint8Array(bytes))
                run()
            }}"#,
        );

        js
    }

    fn as_tokens(&mut self) -> TokenStream2 {
        let all_js = self.js();
        let channel = self.channel();

        let web_entrypoint = {
            #[cfg(feature = "web")]
            {
                let foreign_items = &self.foreign_items;

                let ty = &self.buffer;
                quote! {
                    #[wasm_bindgen::prelude::wasm_bindgen(inline_js = #all_js)]
                    extern "C" {
                        fn create(metadata_ptr: u32);
                        fn run();
                        #[doc = concat!("Runs the serialized message provided")]
                        #[doc = concat!("To create a serialized message, use the [`", stringify!(#ty), "`]::to_bytes` method")]
                        pub fn run_from_buffer(buffer: &[u8]);
                        fn update_memory(memory: wasm_bindgen::JsValue);
                        #(#foreign_items)*
                    }
                }
            }
            #[cfg(not(feature = "web"))]
            {
                quote!()
            }
        };

        quote! {
            #[derive(Default)]
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
            static LAST_MEM_SIZE: std::sync::atomic::AtomicUsize = std::sync::atomic::AtomicUsize::new(0);
            fn set_last_mem_size(size: usize) {
                LAST_MEM_SIZE.store(size, std::sync::atomic::Ordering::SeqCst);
            }
            fn get_last_mem_size() -> usize {
                LAST_MEM_SIZE.load(std::sync::atomic::Ordering::SeqCst)
            }
            #web_entrypoint
            #channel
            const GENERATED_JS: &str = #all_js;
        }
    }

    fn channel(&mut self) -> TokenStream2 {
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
                        *self.msg.get_unchecked_mut(self.current_op_batch_idx + self.current_op_byte_idx / 2) = op;
                    } else {
                        *self.msg.get_unchecked_mut(self.current_op_batch_idx + self.current_op_byte_idx / 2) |= op << 4;
                    }
                    self.current_op_byte_idx += 1;
                }
            }
            8 => {
                quote! {
                    *self.msg.get_unchecked_mut(self.current_op_batch_idx + self.current_op_byte_idx) = op;
                    self.current_op_byte_idx += 1;
                }
            }
            _ => panic!("unsupported size"),
        };

        let ty = &self.buffer;
        let states = self.encoders.iter().map(|(_, e)| {
            let ty = &e.rust_type();
            let ident = &e.rust_ident();
            quote! {
                #ident: #ty,
            }
        });
        let states_default = self.encoders.iter().map(|(_, e)| {
            let ident = &e.rust_ident();
            quote! {
                #ident: Default::default(),
            }
        });

        let pre_run_rust = self
            .encoders
            .iter()
            .map(|(_, e)| e.pre_run_rust())
            .collect::<Vec<_>>();
        let memory_moved_rust = self
            .encoders
            .iter()
            .map(|(_, e)| e.memory_moved_rust())
            .collect::<Vec<_>>();
        let first_run_states = self
            .encoders
            .iter()
            .map(|(_, e)| e.init_rust())
            .collect::<Vec<_>>();
        let post_run_rust = self
            .encoders
            .iter()
            .map(|(_, e)| e.post_run_rust())
            .collect::<Vec<_>>();

        let export_memory_iters = self
            .encoders
            .iter()
            .map(|(ident, e)| {
                // Merge the memory of the #ident encoder
                let comment = quote!(
                    #[doc = concat!(" The memory of the [`", stringify!(#ident), "`] encoder")]
                );

                let merge = e.merge_memory_rust();
                quote! {
                    #comment
                    {
                        let memory: Vec<_> = #merge.collect();
                        let len = memory.len();
                        let iter = memory.into_iter();
                        iter
                    }
                }
            })
            .collect::<Vec<_>>();

        let meta_type = self.encoders.builder.rust_type();
        let meta_ident = self.encoders.builder.rust_ident();
        let meta_init = self.encoders.builder.rust_init();

        let set_msg_ptr = self
            .msg_ptr_u32
            .write_rust(parse_quote! {self.msg.as_ptr() as u32});
        let set_exported_msg_ptr = self.msg_ptr_u32.write_rust(parse_quote! {current_ptr});
        let set_msg_moved = self.msg_moved_flag.write_rust(parse_quote! {msg_moved});
        let get_msg_ptr = self.msg_ptr_u32.get_rust();

        quote! {
            fn __copy(src: &[u8], dst: &mut [u8], len: usize) {
                for (m, i) in dst.iter_mut().zip(src.iter().take(len)) {
                    *m = *i;
                }
            }

            pub struct #ty {
                msg: Vec<u8>,
                current_op_batch_idx: usize,
                current_op_byte_idx: usize,
                #( #states )*
                #meta_ident: #meta_type,
            }

            impl Default for #ty {
                fn default() -> Self {
                    Self {
                        msg: Vec::new(),
                        current_op_batch_idx: 0,
                        current_op_byte_idx: #reads_per_u32,
                        #meta_ident: #meta_init,
                        #( #states_default )*
                    }
                }
            }

            impl #ty {
                pub fn append(&mut self, mut batch: Self) {
                    // add empty operations to the batch to make sure the batch is aligned
                    let operations_left = #reads_per_u32 - self.current_op_byte_idx;
                    for _ in 0..operations_left {
                        self.encode_op(#no_op);
                    }

                    self.current_op_byte_idx = batch.current_op_byte_idx;
                    self.current_op_batch_idx = self.msg.len() + batch.current_op_batch_idx;
                    self.msg.append(&mut batch.msg);
                }

                #[allow(clippy::uninit_vec)]
                fn encode_op(&mut self, op: u8) {
                    unsafe {
                        // SAFETY: this creates 4 bytes of uninitialized memory that will be immediately written to when we encode the operation in the next step
                        if self.current_op_byte_idx >= #reads_per_u32 {
                            self.current_op_batch_idx = self.msg.len();
                            self.msg.reserve(4);
                            self.msg.set_len(self.msg.len() + 4);
                            self.current_op_byte_idx = 0;
                        }
                        // SAFETY: we just have checked that there is enough space in the vector to index into it
                        #encode_op
                    }
                }

                pub fn flush(&mut self){
                    #[cfg(target_family = "wasm")]
                    {
                        self.encode_op(#end_msg);
                        #set_msg_ptr
                        self.update_metadata_ptrs();
                        #(#pre_run_rust)*

                        let new_mem_size = core::arch::wasm32::memory_size(0);
                        // we need to update the memory if the memory has grown
                        if new_mem_size != get_last_mem_size() {
                            set_last_mem_size(new_mem_size);
                            update_memory(wasm_bindgen::memory());
                            #(#memory_moved_rust)*
                        }

                        run();

                        self.reset();
                    }
                }

                pub fn export_memory(&mut self) -> impl Iterator<Item = u8> + '_ {
                    self.encode_op(#end_msg);
                    #(#pre_run_rust)*

                    #(#memory_moved_rust)*
                    let msg_moved = true;
                    #set_msg_moved

                    let msg = &self.msg;
                    let meta = &self.#meta_ident;

                    let meta_iter = meta.iter().flat_map(|i| i.get().to_le_bytes().into_iter());
                    let mut current_ptr = meta.len() as u32 * 4;
                    #set_exported_msg_ptr
                    let iter = msg.iter().copied();
                    current_ptr += msg.len() as u32;
                    #(
                        let iter = iter.chain(#export_memory_iters);
                    )*

                    meta_iter.chain(iter)
                }

                pub fn reset(&mut self){
                    #(#post_run_rust)*
                    self.current_op_batch_idx = 0;
                    self.current_op_byte_idx = #reads_per_u32;
                    self.msg.clear();
                }

                fn update_metadata_ptrs(&mut self) {
                    static FIRST_RUN: std::sync::atomic::AtomicBool = std::sync::atomic::AtomicBool::new(true);
                    let first_run = FIRST_RUN.swap(false, std::sync::atomic::Ordering::Relaxed);
                    let metadata_ptr = self.metadata.as_ref().get_ref() as *const _ as u32;

                    // the pointer will only be updated when the message vec is resized, so we have a flag to check if the pointer has changed to avoid unnecessary decoding
                    if first_run {
                        #(#first_run_states)*
                        // this is the first message, so we need to encode all the metadata
                        #[cfg(target_family = "wasm")]
                        unsafe{
                            // SAFETY: self.metadata is pinned, initialized and we will not write to it while javascript is reading from it
                            create(metadata_ptr);
                        }
                        #set_msg_ptr
                        let msg_moved = true;
                        #set_msg_moved
                    } else {
                        let msg_moved = #get_msg_ptr != metadata_ptr;
                        if msg_moved {
                            #set_msg_ptr
                        }
                        #set_msg_moved
                    }
                }

                #(#methods)*
            }
        }
    }
}
