use std::{any::Any, collections::HashMap, ops::Deref};

use quote::{__private::TokenStream as TokenStream2, quote};
use syn::{Ident, Type};

use crate::builder::BindingBuilder;

pub trait CreateEncoder {
    type Output;

    fn create(&self, builder: &mut BindingBuilder) -> Self::Output;

    fn rust_ident(&self) -> Ident;
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

    fn global_rust(&self) -> TokenStream2 {
        quote!()
    }

    fn init_rust(&self) -> TokenStream2 {
        quote!()
    }

    fn memory_moved_rust(&self) -> TokenStream2 {
        quote!()
    }

    fn pre_run_rust(&self) -> TokenStream2 {
        quote!()
    }

    fn post_run_rust(&self) -> TokenStream2 {
        quote!()
    }
}

pub trait Encode {
    fn encode_js(&self) -> String;

    fn encode_rust(&self, ident: &Ident) -> TokenStream2;
}

pub trait DynEncode: Any + Encode + Encoder {}

impl<T: Any + Encode + Encoder> DynEncode for T {}

pub struct EncodeTraitObject(Box<dyn DynEncode>);

impl Encoder for EncodeTraitObject {
    fn global_js(&self) -> String {
        self.0.global_js()
    }

    fn pre_run_js(&self) -> String {
        self.0.pre_run_js()
    }

    fn rust_type(&self) -> Type {
        self.0.rust_type()
    }

    fn rust_ident(&self) -> Ident {
        self.0.rust_ident()
    }

    fn global_rust(&self) -> TokenStream2 {
        self.0.global_rust()
    }

    fn memory_moved_rust(&self) -> TokenStream2 {
        self.0.memory_moved_rust()
    }

    fn init_rust(&self) -> TokenStream2 {
        self.0.init_rust()
    }

    fn pre_run_rust(&self) -> TokenStream2 {
        self.0.pre_run_rust()
    }

    fn post_run_rust(&self) -> TokenStream2 {
        self.0.post_run_rust()
    }
}

impl Encode for EncodeTraitObject {
    fn encode_js(&self) -> String {
        self.0.encode_js()
    }

    fn encode_rust(&self, ident: &Ident) -> TokenStream2 {
        self.0.encode_rust(ident)
    }
}

#[derive(Default)]
pub struct Encoders {
    encoders: HashMap<Ident, EncodeTraitObject>,
}

impl Deref for Encoders {
    type Target = HashMap<Ident, EncodeTraitObject>;

    fn deref(&self) -> &Self::Target {
        &self.encoders
    }
}

impl Encoders {
    pub fn insert<T: CreateEncoder<Output = O>, O: DynEncode>(
        &mut self,
        factory: T,
        builder: &mut BindingBuilder,
    ) {
        self.get_or_insert_with(factory, builder);
    }

    pub fn get_or_insert_with<T: CreateEncoder<Output = O>, O: DynEncode>(
        &mut self,
        factory: T,
        builder: &mut BindingBuilder,
    ) -> &mut EncodeTraitObject {
        let id = factory.rust_ident();
        self.encoders
            .entry(id)
            .or_insert_with(|| EncodeTraitObject(Box::new(factory.create(builder))))
    }
}
