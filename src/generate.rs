use naga::{
    back::wgsl,
    valid::{Capabilities, ValidationFlags, Validator},
    Module, ShaderStage, TypeInner,
};
use proc_macro2::TokenStream;
use quote::quote;

use crate::{
    constants::const_gen,
    globals::global_gen,
    structs::struct_gen,
    utils::{identify, naga_undecorate},
    Config, Error,
};

pub fn build_entry_tokens() -> TokenStream {
    quote! {
        // from wgsl-bindgen
        #[cfg_attr(debug_assertions, derive(Debug))]
        pub struct FragmentEntry<'__lt> {
            pub entry_point: &'static str,
            pub targets: &'__lt [::core::option::Option<wgpu::ColorTargetState>],
            // pub constants: std::collections::HashMap<String, f64>,
        }

        pub fn make_fragment_state<'__lt>(
            module: &'__lt wgpu::ShaderModule,
            entry: &'__lt FragmentEntry<'__lt>,
        ) -> wgpu::FragmentState<'__lt> {
            wgpu::FragmentState {
                module,
                entry_point: Some(entry.entry_point),
                targets: &entry.targets,
                compilation_options: Default::default(),
            }
        }

        #[cfg_attr(debug_assertions, derive(Debug))]
        pub struct VertexEntry<const N: usize> {
            pub entry_point: &'static str,
            pub buffers: [wgpu::VertexBufferLayout<'static>; N],
            // pub constants: std::collections::HashMap<String, f64>,
        }

        pub fn make_vertex_state<'__lt, const N: usize>(
            module: &'__lt wgpu::ShaderModule,
            entry: &'__lt VertexEntry<N>,
        ) -> wgpu::VertexState<'__lt> {
            wgpu::VertexState {
                module,
                entry_point: Some(entry.entry_point),
                buffers: &entry.buffers,
                compilation_options: Default::default(),
            }
        }
    }
}

fn build_entry_points(
    module: &Module,
    vertex_input_names: Vec<String>,
    config: &Config,
) -> TokenStream {
    let mut fragment_entries = vec![];
    let mut vertex_entries = vec![];

    for entry in &module.entry_points {
        match entry.stage {
            ShaderStage::Fragment => {
                let entry_name = &entry.name;
                let entry_fn_name = identify(&format!("fragment_entry_{}", entry_name));
                let entry_const = identify(&format!("FRAG_ENTRY_{}", entry_name.to_uppercase()));

                fragment_entries.push(quote! {
                    pub const #entry_const: &str = #entry_name;
                    pub fn #entry_fn_name<'__lt>(
                        targets: &'__lt [::core::option::Option<wgpu::ColorTargetState>]
                    ) -> super::super::FragmentEntry<'__lt> {
                        super::super::FragmentEntry {
                            entry_point: #entry_const,
                            targets,
                        }
                    }
                });
            }
            ShaderStage::Vertex => {
                let entry_name = &entry.name;
                let entry_fn_name = identify(&format!("vertex_entry_{}", entry_name));
                let entry_const = identify(&format!("VERT_ENTRY_{}", entry_name.to_uppercase()));

                let entry_n = vertex_input_names.len();

                let mut step_modes = vec![];
                let mut vertex_desc = vec![];

                for name in &vertex_input_names {
                    let name_ident = identify(&format!("{name}_step_mode"));
                    step_modes.push(quote! { #name_ident: wgpu::VertexStepMode });

                    let path = config.resolve_type(name);
                    vertex_desc.push(quote! { #path::vertex_desc(#name_ident) })
                }

                vertex_entries.push(quote! {
                    pub const #entry_const: &str = #entry_name;
                    #[allow(non_snake_case)]
                    pub fn #entry_fn_name(#(#step_modes),*) -> super::super::VertexEntry<#entry_n> {
                        super::super::VertexEntry {
                            entry_point: #entry_const,
                            buffers: [
                                #(#vertex_desc),*
                            ],
                            // constants: #constants
                        }
                    }
                })
            }
            f => todo!("other shader stages {f:?}"),
        }
    }

    quote! {
        pub mod entries {
            #(#fragment_entries)*
            #(#vertex_entries)*
        }
    }
}

fn build_pipeline_func(indexes: &[u32], config: &Config) -> TokenStream {
    let label = format!("{}::PipelineLayout", &config.file_name);

    let mut bind_groups = vec![];

    for index in indexes {
        let path = config.bind_group_location.get(&(*index as usize));
        // only 1 super since we are already at the root of the file/mod
        bind_groups.push(quote! {
            super:: #path :: get_bind_group_layout(device)
        });
    }

    quote! {
        pub fn create_pipeline_layout(device: &wgpu::Device) -> wgpu::PipelineLayout {
            device
                .create_pipeline_layout(
                    &wgpu::PipelineLayoutDescriptor {
                        label: Some(#label),
                        bind_group_layouts: &[
                            #(&#bind_groups),*
                        ],
                        push_constant_ranges: &[],
                    },
                )
        }
    }
}

pub fn generate(module: &Module, config: &mut Config) -> Result<TokenStream, Error> {
    let (globals_tokens, mut bind_group_indexes) =
        global_gen::make_global_bindgroups(module, config);
    let constants_tokens = const_gen::make_constants(module, config);
    let (structs_tokens, vertex_inputs) = struct_gen::make_structs(module, config);

    let entries_tokens = build_entry_points(module, vertex_inputs, config);

    let info = Validator::new(ValidationFlags::empty(), Capabilities::all())
        .validate(module)
        .map_err(|e| Error::NagaValidationError(e.as_inner().clone()))?;

    let source = wgsl::write_string(module, &info, wgsl::WriterFlags::empty())
        .map_err(Error::NagaWriterError)?;

    let shader_module_name = format!("{}::ShaderModule", &config.file_name);

    bind_group_indexes.sort();
    let pipeline_func = build_pipeline_func(&bind_group_indexes, config);

    let shared_tokens = quote! {
        pub const SOURCE: &str = #source;

        #pipeline_func

        pub fn create_shader_module(device: &wgpu::Device) -> wgpu::ShaderModule {
            device.create_shader_module(wgpu::ShaderModuleDescriptor {
                label: Some(#shader_module_name),
                source: wgpu::ShaderSource::Wgsl(self::SOURCE.into()),
            })
        }
    };

    if config.separate_files {
        Ok(quote! {
            #globals_tokens
            #constants_tokens
            #structs_tokens
            #entries_tokens

            #shared_tokens
        })
    } else {
        let mod_name = identify(&config.file_name);

        // we only ever want 1 instance of these structs so in a single file they go at the top
        let entry_structs = build_entry_tokens();

        Ok(quote! {
            #entry_structs

            pub mod #mod_name {
                #globals_tokens
                #constants_tokens
                #structs_tokens
                #entries_tokens

                #shared_tokens
            }
        })
    }
}
