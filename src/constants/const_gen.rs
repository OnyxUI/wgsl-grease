use naga::Module;
use proc_macro2::TokenStream;
use quote::quote;

use crate::{
    utils::{identify, naga_undecorate},
    Config,
};

pub fn make_constants(module: &Module, config: &mut Config) -> TokenStream {
    let mut tokens = TokenStream::new();

    for (_, constant) in module.constants.iter() {
        let name_str = naga_undecorate(constant.name.as_ref().unwrap());

        if config.defined_constants.contains(name_str) {
            continue;
        } else {
            config.defined_constants.insert(name_str.to_string());
        }

        // TODO: demangled names
        // let rust_item_path = RustItemPath::from_mangled(name_str, invoking_entry_module);
        let name = identify(name_str);

        // TODO: support f16 once naga supports them
        let type_and_value = match &module.global_expressions[constant.init] {
            naga::Expression::Literal(literal) => match literal {
                naga::Literal::F32(v) => Some(quote! { f32 = #v }),
                naga::Literal::F64(v) => Some(quote! { f64 = #v }),
                naga::Literal::U32(v) => Some(quote! { u32 = #v }),
                naga::Literal::U64(v) => Some(quote! { u64 = #v }),
                naga::Literal::I32(v) => Some(quote! { i32 = #v }),
                naga::Literal::I64(v) => Some(quote! { i64 = #v }),
                naga::Literal::Bool(v) => Some(quote! { bool = #v }),
                naga::Literal::AbstractInt(v) => Some(quote! { i64 = #v }),
                naga::Literal::AbstractFloat(v) => Some(quote! { f64 = #v }),
            },
            _ => continue,
        };

        tokens.extend(quote! { pub const #name: #type_and_value; });
    }

    quote! {
        pub mod constants {
            #[allow(unused_imports)]
            pub use super::*;

            #tokens
        }
    }
}
