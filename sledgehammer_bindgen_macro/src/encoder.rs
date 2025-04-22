use std::{any::Any, collections::BTreeMap, ops::Deref, rc::Rc};

use quote::{__private::TokenStream as TokenStream2, quote};
use syn::{Ident, Type};

use crate::builder::BindingBuilder;

pub trait CreateEncoder {
    type Output;

    fn create(&self, encoder: &mut Encoders) -> Self::Output;

    fn rust_ident(&self) -> Ident;
}

pub trait Encoder {
    fn initializer(&self) -> String {
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

    fn merge_memory_rust(&self) -> TokenStream2 {
        quote! {
            std::iter::empty()
        }
    }
}

pub trait Encode {
    fn encode_js(&self) -> String;

    fn encode_rust(&self, ident: &Ident) -> TokenStream2;
}

pub trait DynEncode: Any + Encode + Encoder {}

impl<T: Any + Encode + Encoder> DynEncode for T {}

pub trait AnyDynEncode: DynEncode {
    fn as_any(&self) -> &dyn Any;
}

impl<T: DynEncode> AnyDynEncode for T {
    fn as_any(&self) -> &dyn Any {
        self
    }
}

#[derive(Clone)]
pub struct EncodeTraitObject(Rc<dyn AnyDynEncode>);

impl EncodeTraitObject {
    pub fn downcast<T: AnyDynEncode>(&self) -> &T {
        let rc_any = &*self.0;
        rc_any.as_any().downcast_ref::<T>().unwrap()
    }
}

impl Encoder for EncodeTraitObject {
    fn initializer(&self) -> String {
        self.0.initializer()
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

    fn merge_memory_rust(&self) -> TokenStream2 {
        self.0.merge_memory_rust()
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
    encoders: BTreeMap<Ident, EncodeTraitObject>,
    pub(crate) builder: BindingBuilder,
}

impl Deref for Encoders {
    type Target = BTreeMap<Ident, EncodeTraitObject>;

    fn deref(&self) -> &Self::Target {
        &self.encoders
    }
}

impl Encoders {
    pub fn insert<T: CreateEncoder<Output = O>, O: AnyDynEncode>(&mut self, factory: T) {
        self.get_or_insert_with(factory);
    }

    pub fn get_or_insert_with<T: CreateEncoder<Output = O>, O: AnyDynEncode>(
        &mut self,
        factory: T,
    ) -> EncodeTraitObject {
        let id = factory.rust_ident();
        let value = self.encoders.get(&id);
        match value {
            Some(value) => value.clone(),
            None => {
                let value = EncodeTraitObject(Rc::new(factory.create(self)));
                self.encoders.insert(id.clone(), value.clone());
                value
            }
        }
    }

    pub fn builder(&mut self) -> &mut BindingBuilder {
        &mut self.builder
    }
}
