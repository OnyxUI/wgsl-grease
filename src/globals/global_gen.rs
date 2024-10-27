use std::collections::HashMap;

use naga::{Module, Type, TypeInner};
use proc_macro2::TokenStream;
use quote::quote;

use crate::{
    utils::{identify, naga_undecorate},
    Config,
};

use super::bind_group_gen::{bind_group_layout_entry_type, custom_bind_group_layout_entry_type};

fn naga_stage_to_wgpu(module: &naga::Module) -> wgpu_types::ShaderStages {
    module
        .entry_points
        .iter()
        .map(|entry| match entry.stage {
            naga::ShaderStage::Vertex => wgpu_types::ShaderStages::VERTEX,
            naga::ShaderStage::Fragment => wgpu_types::ShaderStages::FRAGMENT,
            naga::ShaderStage::Compute => panic!("compute not supported"),
        })
        .collect()
}

#[derive(Debug, PartialEq, Eq, Clone, Copy, Hash)]
pub enum BindResourceType {
    Buffer,
    Sampler,
    Texture,
}

// some of this code is generously inspired by wgsl-bindgen as well as naga-to-tokenstream
#[derive(Clone, Debug)]
struct GroupBinding {
    name: String,
    index: u32,
    typ: Type,
    address_space: naga::AddressSpace,
    resource_type: BindResourceType,
}

fn make_group_bindings(module: &Module) -> HashMap<u32, Vec<GroupBinding>> {
    let mut global_bindings: HashMap<u32, Vec<GroupBinding>> = HashMap::new();

    for (_, global) in module.global_variables.iter() {
        let global_name = match &global.name {
            Some(n) => naga_undecorate(n).to_string(),
            None => {
                println!("warn: skipping as missing global name");
                continue;
            }
        };

        let global_binding = match &global.binding {
            Some(b) => b.clone(),
            None => {
                println!("warn: skipping as missing global binding");
                continue;
            }
        };

        //match WgslType::from_naga(
        let global_type = module
            .types
            .get_handle(global.ty)
            .expect("failed to get type")
            .clone();
        //     module,
        // ) {
        //     Some(t) => t,
        //     None => {
        //         println!("warn: skipping as invalid type");
        //         continue;
        //     }
        // };

        let group_binding = GroupBinding {
            resource_type: match &global_type.inner {
                TypeInner::Scalar { .. } | TypeInner::Array { .. } | TypeInner::Struct { .. } => {
                    BindResourceType::Buffer
                }
                TypeInner::Image { .. } => BindResourceType::Texture,
                TypeInner::Sampler { .. } => BindResourceType::Sampler,
                _ => panic!("unknown BindResourceType"),
            },
            name: global_name,
            index: global_binding.binding,
            typ: global_type,
            address_space: global.space,
        };

        global_bindings
            .entry(global_binding.group)
            .or_default()
            .push(group_binding);
    }

    global_bindings
}

pub fn make_global_bindgroups(module: &Module, config: &mut Config) -> TokenStream {
    let mut globals = TokenStream::new();

    let global_bindings = make_group_bindings(module);

    let visibility = naga_stage_to_wgpu(module);

    let mut binding_names = vec![];

    for bindex in global_bindings.keys() {
        let bindings = global_bindings.get(bindex).unwrap();

        // TODO: if the vec contains any extra globals that dont exist instead of regenerating the whole struct it should maybe make something like
        /*
           struct BindGroupX1Entries {
               pub base: BindGroupXEntries,
               pub X: wgpu::BindGroupEntry<...>,
               ...
           }
        */
        // if we have already encountered this bind group and all of its members, skip
        if let Some(known_bindings) = config.defined_globals.get(&(*bindex as usize)) {
            if bindings
                .iter()
                .all(|binding| known_bindings.contains(&binding.name))
            {
                continue;
            }
        }

        let mut entries_params_fields = vec![];
        let mut entries_fields = vec![];
        let mut entries_field_names = vec![];
        let mut bind_group_entries = vec![];
        let mut bind_group_entry_layouts = vec![];

        for binding in bindings {
            let GroupBinding {
                name,
                index,
                typ,
                address_space,
                resource_type,
            } = binding;

            binding_names.push(name.clone());

            let name_ident = identify(naga_undecorate(name));

            // generates the fields in the BindGroupXEntriesParams
            let entry_param_type = match resource_type {
                BindResourceType::Buffer => quote! { wgpu::BufferBinding<'__lt> },
                BindResourceType::Sampler => quote! { &'__lt wgpu::Sampler },
                BindResourceType::Texture => quote! { &'__lt wgpu::TextureView },
            };

            entries_params_fields.push(quote! {
                pub #name_ident : #entry_param_type
            });

            // generates the fields in the BindGroupXEntries
            entries_fields.push(quote! {
                pub #name_ident : wgpu::BindGroupEntry<'__lt>
            });
            entries_field_names.push(quote! { #name_ident });

            // generates the values for the fields in the BindGroupXEntries (for the `new` implementation)
            let binding_resource_type = match resource_type {
                BindResourceType::Buffer => quote! { wgpu::BindingResource::Buffer },
                BindResourceType::Sampler => quote! { wgpu::BindingResource::Sampler },
                BindResourceType::Texture => quote! { wgpu::BindingResource::TextureView },
            };

            bind_group_entries.push(quote! {
                #name_ident : wgpu::BindGroupEntry {
                    binding: #index,
                    resource: #binding_resource_type (params . #name_ident)
                }
            });

            // generates the entries for the BindGroupLayoutDescriptor
            let bind_group_layout_entry_doc = format!("@binding({bindex}): {name}");

            let entry = if let Some(func) = config.bg_layout_entry_overrides.get(name) {
                custom_bind_group_layout_entry_type(module, typ, *index, visibility, func, config)
            } else {
                bind_group_layout_entry_type(module, typ, *index, visibility, address_space, config)
            };

            bind_group_entry_layouts.push(quote! {
                #[doc = #bind_group_layout_entry_doc]
                #entry
            });
        }

        let bind_group_name = identify(&format!("BindGroup{bindex}"));
        let bind_group_entries_name = identify(&format!("{bind_group_name}Entries"));
        let bind_group_entries_params_name =
            identify(&format!("{bind_group_entries_name}EntriesParams"));

        let bind_group_layout_descriptor_label =
            format!("{}::BindGroup{bindex}::LayoutDescriptor", &config.file_name);

        let bind_group_descriptor_label = format!("{}::BindGroup{bindex}", &config.file_name);

        globals.extend(quote! {
            #[cfg_attr(debug_assertions, derive(Debug))]
            #[allow(non_snake_case)]
            pub struct #bind_group_entries_params_name <'__lt> {
                #(#entries_params_fields),*
            }

            #[cfg_attr(debug_assertions, derive(Debug))]
            #[allow(non_snake_case)]
            pub struct #bind_group_entries_name <'__lt> {
                #(#entries_fields),*
            }
            impl<'__lt> #bind_group_entries_name <'__lt> {
                pub fn new(params: #bind_group_entries_params_name <'__lt>) -> Self {
                    Self { #(#bind_group_entries),* }
                }
            }

            #[cfg_attr(debug_assertions, derive(Debug))]
            #[allow(non_snake_case)]
            pub struct #bind_group_name (wgpu::BindGroup);
            impl #bind_group_name  {
                pub const INDEX: u32 = #bindex;

                #[allow(unused_doc_comments)]
                pub const LAYOUT_DESCRIPTOR: wgpu::BindGroupLayoutDescriptor<'static> =  wgpu::BindGroupLayoutDescriptor {
                    label: Some(#bind_group_layout_descriptor_label),
                    entries: &[
                        #(#bind_group_entry_layouts),*
                    ],
                };

                pub fn get_bind_group_layout(device: &wgpu::Device) -> wgpu::BindGroupLayout {
                    device.create_bind_group_layout(&Self::LAYOUT_DESCRIPTOR)
                }

                pub fn from_bindings(
                    device: &wgpu::Device,
                    bindings: #bind_group_entries_name,
                ) -> Self {
                    let bind_group_layout = Self::get_bind_group_layout(&device);
                    let bind_group = device
                        .create_bind_group(
                            &wgpu::BindGroupDescriptor {
                                label: Some(#bind_group_descriptor_label),
                                layout: &bind_group_layout,
                                entries: &[
                                    #(bindings. #entries_field_names),*
                                ],
                            },
                        );
                    Self(bind_group)
                }

                pub fn get_bind_group(&self) -> &wgpu::BindGroup {
                    &self.0
                }
            }
        });

        config
            .defined_globals
            .entry(*bindex as usize)
            .or_insert(binding_names.clone());
        binding_names.clear();
    }

    quote! {
        pub mod globals {
            #[allow(unused_imports)]
            pub use super::*;

            #globals
        }
    }
}
