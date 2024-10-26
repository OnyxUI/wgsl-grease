use proc_macro2::{Ident, Span, TokenStream};

use crate::Config;

pub mod rust_type_info;

pub fn naga_undecorate(string: &str) -> &str {
    string
        .split_once("X_naga_oil_mod_X")
        .map_or_else(|| string, |(undecorated, ..)| undecorated)
}

/// convert string to ident
pub fn identify(string: &str) -> Ident {
    Ident::new(string, Span::call_site())
}

pub fn quote_visibility(vis: wgpu_types::ShaderStages) -> TokenStream {
    match vis {
        wgpu_types::ShaderStages::VERTEX => quote::quote! { wgpu::ShaderStages::VERTEX },
        wgpu_types::ShaderStages::FRAGMENT => quote::quote! { wgpu::ShaderStages::FRAGMENT },
        wgpu_types::ShaderStages::VERTEX_FRAGMENT => {
            quote::quote! { wgpu::ShaderStages::VERTEX_FRAGMENT }
        }
        wgpu_types::ShaderStages::NONE => {
            quote::quote! { wgpu::ShaderStages::NONE }
        }
        stage => unimplemented!("implement shader stage {stage:?}"),
    }
}

impl Config {
    pub fn resolve_type(&self, name: &str) -> TokenStream {
        if let Some(path) = self.resolve_type_map.get(name) {
            // double super moves out of the `structs` module and then the parent to that
            // so the path resolves correctly
            quote::quote! { super::super:: #path }
        } else {
            let name = identify(name);
            // all imports are processed first so if the struct is not already in the map
            // chances are its in our file and may have not been processed yet
            // otherwise if its not there its just not defined

            quote::quote! { super::structs::#name }
        }
    }
}
