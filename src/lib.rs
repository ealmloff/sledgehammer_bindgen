use std::collections::HashSet;

use proc_macro::TokenStream;
use quote::__private::{Span, TokenStream as TokenStream2};
use quote::quote;
use syn::{
    parse::Parse, parse_macro_input, Expr, GenericArgument, Ident, Lit, Pat, PathArguments, Type,
};

#[proc_macro_attribute]
pub fn bindgen(_: TokenStream, input: TokenStream) -> TokenStream {
    let mut input = parse_macro_input!(input as Bindings);

    input.as_tokens().into()
}

#[derive(Debug)]
struct Bindings {
    functions: Vec<FunctionBinding>,
    intialize: Option<FunctionBinding>,
}

impl Parse for Bindings {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        let extren_block = syn::ItemForeignMod::parse(input)?;

        let mut functions = Vec::new();
        let mut intialize = None;
        for item in extren_block.items {
            match item {
                syn::ForeignItem::Verbatim(s) => {
                    let f: FunctionBinding = syn::parse2(s).unwrap();
                    if f.name == "initialize" {
                        intialize = Some(f);
                    } else {
                        functions.push(f);
                    }
                }
                _ => panic!("only functions are supported"),
            }
        }

        Ok(Bindings {
            functions,
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

fn select_bits_js(input: &str, pos: usize, len: usize) -> String {
    if len == 32 {
        assert!(pos == 0);
    }
    assert!(len <= 32);
    let mut s = String::new();

    if pos != 0 {
        s += &format!("{}>>>{}", input, pos);
    } else {
        s += input;
    }

    if pos + len < 32 {
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
        let initialize = self.intialize.as_mut().map(|f| f.js()).unwrap_or_default();
        let start = format!(
            r#"let m,p,ls,lss,sp,d,t,c,s,sl,op,i,e,{};{}export function create(r){{d=r;c=new TextDecoder()}}export function update_memory(r){{m=new DataView(r.buffer)}}export function set_buffer(b){{m=new DataView(b)}}export function run(){{t=m.getUint8(d,true);if(t&1){{ls=m.getUint32(d+1,true)}}p=ls;if(t&2){{lss=m.getUint32(d+5,true)}}if(t&4){{sl=m.getUint32(d+9,true);if(t&8){{sp=lss;s="";e=sp+(sl/4|0)*4;while(sp<e){{t=m.getUint32(sp,true);s+=String.fromCharCode(t>>24,(t&16711680)>>16,(t&65280)>>8,t&255);sp+=4}}switch(lss+sl-sp){{case 3:t=m.getUint32(sp,true);s+=String.fromCharCode(t>>24,(t&16711680)>>16,(t&65280)>>8);break;case 2:t=m.getUint16(sp,true);s+=String.fromCharCode(t>>8,t&255);break;case 1:s+=String.fromCharCode(m.getUint8(sp),true);break;case 0:break}}}}else{{s=c.decode(new DataView(m.buffer,lss,sl))}}sp=0}}for(;;){{op=m.getUint32(p,true);p+=4;{}}}}}function exOp(){{switch (op & {}) {{"#,
            self.variables_js(),
            initialize,
            self.read_operations_js(),
            with_n_1_bits(op_size)
        );
        self.functions
            .iter_mut()
            .enumerate()
            .filter(|f| f.1.name != "initialize")
            .fold(start, |s, (i, f)| {
                s + &format!("case {}:{}break;", i, f.js())
            })
            + &format!("case {}:return true;}}}}", self.functions.len())
    }

    fn read_operations_js(&self) -> String {
        let mut s = String::new();
        let size = function_discriminant_size_bits(self.functions.len() as u32);
        assert!(size <= 8);
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

    fn as_tokens(&mut self) -> TokenStream2 {
        let all_js = self.js();
        let channel = self.channel();
        quote! {
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
            }
            #channel
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

                #[cfg(target_family = "wasm")]
                pub fn flush(&mut self){
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
                            *METADATA = 3;
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
                                *METADATA |= (self.str_buffer.is_ascii() as u8) << 4;
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
                }

                #(#methods)*
            }
        }
    }
}

#[derive(Debug)]
struct FunctionBinding {
    name: Ident,
    args: Vec<(Ident, SupportedTypes)>,
    body: String,
    bins: Vec<Bin<(Ident, SupportedTypes)>>,
    remaining: Vec<(Ident, SupportedTypes)>,
}

impl FunctionBinding {
    fn js(&mut self) -> String {
        let (bins, remaining) = pack(self.args.as_slice(), 4);

        self.bins = bins;
        self.remaining = remaining;

        let mut s = String::new();

        for b in &self.bins {
            s += "i=m.getUint32(p,true);";
            s += &b.js();
        }

        for (name, ty) in &self.remaining {
            s += "i=m.getUint32(p,true);";
            s += &ty.js(name.to_string(), 0);
        }

        s += &self.body;

        s
    }

    fn to_tokens(&self, index: u8) -> TokenStream2 {
        let name = &self.name;
        let args = self.args.iter().map(|(a, _)| a);
        let types = self.args.iter().map(|(_, t)| t.to_tokens());
        let encode_types = self
            .bins
            .iter()
            .flat_map(|bin| bin.filled.iter().map(|entry| &entry.item))
            .chain(self.remaining.iter())
            .map(|(i, t)| t.encode(i));
        let size: usize = self.args.iter().map(|(_, t)| t.min_size()).sum();
        quote! {
            #[allow(clippy::uninit_vec)]
            pub fn #name(&mut self, #(#args: #types),*) {
                self.encode_op(#index);
                self.msg.reserve(#size);
                unsafe{
                    #(
                        #encode_types;
                    )*
                }
            }
        }
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

        Ok(Self {
            name,
            args,
            body,
            bins: Vec::new(),
            remaining: Vec::new(),
        })
    }
}

#[derive(Debug, Clone)]
#[allow(unused)]
enum SupportedTypes {
    Optional(SimpleTypes),
    SimpleTypes(SimpleTypes),
}

impl SupportedTypes {
    fn sized(&self) -> bool {
        match self {
            SupportedTypes::Optional(t) => t.sized(),
            SupportedTypes::SimpleTypes(t) => t.sized(),
        }
    }

    fn min_size(&self) -> usize {
        match self {
            SupportedTypes::Optional(t) => t.min_size() + 1,
            SupportedTypes::SimpleTypes(t) => t.min_size(),
        }
    }

    fn js(&self, parameter: String, pos: usize) -> String {
        match self {
            SupportedTypes::Optional(_) => todo!(),
            SupportedTypes::SimpleTypes(t) => t.js(parameter, pos),
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
    Str { size_type: Number },
}

impl SimpleTypes {
    fn sized(&self) -> bool {
        match self {
            SimpleTypes::Number(_) => true,
            SimpleTypes::Slice(_) => false,
            SimpleTypes::Str { .. } => true,
        }
    }

    fn min_size(&self) -> usize {
        match self {
            SimpleTypes::Number(n) => n.size(),
            SimpleTypes::Slice(s) => s.len_size_bytes(),
            SimpleTypes::Str { size_type } => size_type.size(),
        }
    }

    fn js(&self, parameter: String, pos: usize) -> String {
        match self {
            SimpleTypes::Number(n) => n.js(parameter, pos),
            SimpleTypes::Slice(s) => s.js(parameter, pos),
            SimpleTypes::Str { size_type } => {
                format!(
                    "{}=s.substring(sp, sp += {});",
                    parameter,
                    size_type.js_get(pos)
                )
            }
        }
    }

    fn to_tokens(&self) -> TokenStream2 {
        match self {
            SimpleTypes::Number(n) => n.to_tokens(),
            SimpleTypes::Slice(s) => s.to_tokens(),
            SimpleTypes::Str { .. } => quote! { &str },
        }
    }

    fn encode(&self, name: &Ident) -> TokenStream2 {
        match self {
            SimpleTypes::Number(n) => n.encode(name),
            SimpleTypes::Slice(s) => s.encode(name),
            SimpleTypes::Str { size_type } => {
                let len_byte_size = size_type.size();
                let len = Ident::new("len", Span::call_site());
                let write_len = size_type.encode_write(&len);
                quote! {
                    let #len = #name.len();
                    #write_len
                    self.msg.set_len(self.msg.len() + #len_byte_size);
                    self.str_buffer.reserve(#len);
                    let old_len = self.str_buffer.len();
                    unsafe {
                        let ptr = self.str_buffer.as_mut_ptr();
                        let bytes = #name.as_bytes();
                        let str_ptr = bytes.as_ptr();
                        for o in 0..#len {
                            *ptr.add(old_len + o) = *str_ptr.add(o);
                        }
                        self.str_buffer.set_len(old_len + #len);
                    }
                }
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
                            if let &[GenericArgument::Type(Type::Path(t))] = generics.as_slice() {
                                let segments: Vec<_> = t.path.segments.iter().collect();
                                if let &[simple] = segments.as_slice() {
                                    return SupportedTypes::SimpleTypes(SimpleTypes::Str {
                                        size_type: match simple.ident.to_string().as_str() {
                                            "u8" => Number::U8,
                                            "u16" => Number::U16,
                                            "u24" => Number::U24,
                                            "u32" => Number::U32,
                                            _ => panic!("unsupported type"),
                                        },
                                    });
                                }
                            }
                        } else {
                            return SupportedTypes::SimpleTypes(SimpleTypes::Str {
                                size_type: Number::U32,
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
    fn len_size_bytes(&self) -> usize {
        self.size.size()
    }

    // the position must be up to date before calling this
    fn js(&self, parameter: String, pos: usize) -> String {
        let size_bytes = self.len_size_bytes();
        let read = select_bits_js("i", pos, size_bytes * 8);
        match self.inner {
            Number::U8 => format!(
                "p+={};{}=new Uint8Array(m.buffer, p, {});p+={};",
                size_bytes, parameter, read, read
            ),
            Number::U16 => format!(
                "p+={};{}=new Uint16Array(m.buffer, p, {});p+={};",
                size_bytes, parameter, read, read
            ),
            Number::U32 => format!(
                "p+={};{}=new Uint32Array(m.buffer, p, {});p+={};",
                size_bytes, parameter, read, read
            ),
            _ => todo!(),
        }
    }

    fn to_tokens(&self) -> TokenStream2 {
        if self.inner != Number::U8 {
            todo!("fix alignment issues");
        }
        let inner = self.inner.to_tokens();
        quote! { &[#inner] }
    }

    fn encode(&self, name: &Ident) -> TokenStream2 {
        let len = Ident::new("len", Span::call_site());
        let encode_len = self.size.encode(&len);
        let ty = self.inner.to_tokens();
        quote! {
            let #len = #name.len();
            #encode_len
            self.msg.reserve(#len);
            let msg_ptr = self.msg.as_mut_ptr();
            let slice_ptr = #name.as_ptr();
            let old_len = self.msg.len();
            unsafe{
                for i in 0..#len {
                    *msg_ptr.add(old_len).cast::<#ty>().add(i) = *slice_ptr.add(i);
                }
            }
            self.msg.set_len(old_len + #len);
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

    fn js(&self, parameter: String, pos: usize) -> String {
        format!("{}={};", parameter, self.js_get(pos))
    }

    fn js_get(&self, pos: usize) -> String {
        match self {
            Number::U8 => select_bits_js("i", pos, 8),
            Number::U16 => select_bits_js("i", pos, 16),
            Number::U24 => select_bits_js("i", pos, 24),
            Number::U32 => select_bits_js("i", pos, 32),
        }
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
        if let Number::U24 = self {
            quote! {
                let as_u32 = u32::from(#name);
                *self.msg.as_mut_ptr().add(self.msg.len()).cast() = as_u32;
            }
        } else {
            quote! {
                *self.msg.as_mut_ptr().add(self.msg.len()).cast() = #name;
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
        js += &self.position.to_string();
        js += ";";

        // let variables: Vec<_> = self
        //     .filled
        //     .iter()
        //     .map(|entry| format!("{}", entry.item.0))
        //     .collect();
        // js += &variables.join(",");
        // js += "=";
        // let values: Vec<_> = self
        //     .filled
        //     .iter()
        //     .map(|entry| entry.item.1.js(entry.position * 8))
        //     .collect();
        // js += &values.join(",");
        for entry in &self.filled {
            js += &entry
                .item
                .1
                .js(entry.item.0.to_string(), entry.position * 8);
        }

        js
    }
}

#[derive(Debug)]
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
        self.1.sized()
    }

    fn size(&self) -> usize {
        self.1.min_size()
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
