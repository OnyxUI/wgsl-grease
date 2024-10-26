use naga::{
    back::wgsl,
    valid::{Capabilities, ValidationFlags, Validator},
    Module,
};
use proc_macro2::TokenStream;
use quote::quote;

use crate::{
    constants::const_gen, globals::global_gen, structs::struct_gen, utils::identify, Config, Error,
};

pub fn generate(module: &Module, config: &mut Config) -> Result<TokenStream, Error> {
    let globals_tokens = global_gen::make_global_bindgroups(module, config);
    let constants_tokens = const_gen::make_constants(module, config);
    let structs_tokens = struct_gen::make_structs(module, config);

    let info = Validator::new(ValidationFlags::empty(), Capabilities::all())
        .validate(module)
        .map_err(|e| Error::NagaValidationError(e.as_inner().clone()))?;

    let source = wgsl::write_string(module, &info, wgsl::WriterFlags::empty())
        .map_err(Error::NagaWriterError)?;

    if config.separate_files {
        Ok(quote! {
            #globals_tokens
            #constants_tokens
            #structs_tokens

            pub const SOURCE: &str = #source;
        })
    } else {
        let mod_name = identify(&config.file_name);
        Ok(quote! {
            pub mod #mod_name {
                #globals_tokens
                #constants_tokens
                #structs_tokens

                pub const SOURCE: &str = #source;
            }
        })
    }
}
