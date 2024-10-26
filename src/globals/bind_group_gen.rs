use naga::{AddressSpace, Module, StorageAccess, StorageFormat, Type, TypeInner};
use proc_macro2::TokenStream;
use quote::quote;

use crate::{
    utils::{identify, quote_visibility, rust_type_info::rust_type},
    BindGroupLayoutEntryFunc, Config,
};

fn quote_view_dimension(view_dim: wgpu_types::TextureViewDimension) -> TokenStream {
    match view_dim {
        wgpu_types::TextureViewDimension::D1 => quote! { wgpu::TextureViewDimension::D1 },
        wgpu_types::TextureViewDimension::D2 => quote! { wgpu::TextureViewDimension::D2 },
        wgpu_types::TextureViewDimension::D2Array => quote! { wgpu::TextureViewDimension::D2Array },
        wgpu_types::TextureViewDimension::Cube => quote! { wgpu::TextureViewDimension::Cube },
        wgpu_types::TextureViewDimension::CubeArray => {
            quote! { wgpu::TextureViewDimension::CubeArray }
        }
        wgpu_types::TextureViewDimension::D3 => quote! { wgpu::TextureViewDimension::D3 },
    }
}

// from a user-generated layout entry
pub fn custom_bind_group_layout_entry_type(
    module: &Module,
    typ: &Type,
    index: u32,
    visibility: wgpu_types::ShaderStages,
    func: &BindGroupLayoutEntryFunc,
    config: &Config,
) -> TokenStream {
    let custom_entry = func(index, visibility);

    let wgpu_types::BindGroupLayoutEntry {
        binding,
        visibility,
        ty,
        count,
    } = custom_entry;

    let visibility = quote_visibility(visibility);

    let rust_type = rust_type(module, typ, config)
        .expect("unknown rust type (this will be handled at a later date)");
    let min_binding_size = rust_type.quote_min_binding_size();

    let typ = match ty {
        wgpu_types::BindingType::Buffer {
            ty,
            has_dynamic_offset,
            ..
        } => {
            let ty = match ty {
                wgpu_types::BufferBindingType::Uniform => {
                    quote! { wgpu::BufferBindingType::Uniform }
                }
                wgpu_types::BufferBindingType::Storage { read_only } => {
                    quote! { wgpu::BufferBindingType::Storage { read_only: #read_only } }
                }
            };
            quote! {
                wgpu::BindingType::Buffer {
                    ty: #ty,
                    has_dynamic_offset: #has_dynamic_offset,
                    min_binding_size: #min_binding_size,
                }
            }
        }
        wgpu_types::BindingType::Sampler(sampler_binding_type) => {
            let ty = match sampler_binding_type {
                wgpu_types::SamplerBindingType::Filtering => {
                    quote! { wgpu::SamplerBindingType::Filtering }
                }
                wgpu_types::SamplerBindingType::NonFiltering => {
                    quote! { wgpu::SamplerBindingType::NonFiltering }
                }
                wgpu_types::SamplerBindingType::Comparison => {
                    quote! { wgpu::SamplerBindingType::Comparison }
                }
            };
            quote! {
                wgpu_types::BindingType::Sampler(#ty)
            }
        }
        wgpu_types::BindingType::Texture {
            sample_type,
            view_dimension,
            multisampled,
        } => {
            let sample_ty = match sample_type {
                wgpu_types::TextureSampleType::Float { filterable } => {
                    quote! { wgpu::TextureSampleTypes::Float { filterable: #filterable} }
                }
                wgpu_types::TextureSampleType::Depth => quote! { wgpu::TextureSampleTypes::Depth },
                wgpu_types::TextureSampleType::Sint => quote! { wgpu::TextureSampleTypes::Sint },
                wgpu_types::TextureSampleType::Uint => quote! { wgpu::TextureSampleTypes::Uint },
            };

            let view_dimension = quote_view_dimension(view_dimension);

            quote! {
                wgpu::BindingType::Texture {
                    sample_ty: #sample_ty,
                    view_dimension: #view_dimension,
                    multisampled: #multisampled,
                }
            }
        }
        wgpu_types::BindingType::StorageTexture {
            access,
            format,
            view_dimension,
        } => {
            let access = match access {
                wgpu_types::StorageTextureAccess::WriteOnly => {
                    quote! { wgpu::StorageTextureAccess::WriteOnly }
                }
                wgpu_types::StorageTextureAccess::ReadOnly => {
                    quote! { wgpu::StorageTextureAccess::ReadOnly }
                }
                wgpu_types::StorageTextureAccess::ReadWrite => {
                    quote! { wgpu::StorageTextureAccess::ReadWrite }
                }
            };

            let format_ident = identify(&format!("{format:?}"));
            let format = quote! { #format_ident };

            let view_dimension = quote_view_dimension(view_dimension);

            quote! {
                wgpu::BindingType::StorageTexture {
                    access: #access,
                    format: #format,
                    view_dimension: #view_dimension,
                }
            }
        }
        wgpu_types::BindingType::AccelerationStructure => {
            quote! { wgpu::BindingType::AccelerationStructure }
        }
    };

    let count = match count {
        Some(c) => {
            let num: u32 = c.into();
            quote! { std::num::NonZeroU32::new(#num) }
        }
        None => quote! { None },
    };

    quote! {
        wgpu::BindGroupLayoutEntry {
            binding: #binding,
            visibility: #visibility,
            ty: #typ,
            count: #count,
        }
    }
}

pub fn bind_group_layout_entry_type(
    module: &Module,
    typ: &Type,
    index: u32,
    visibility: wgpu_types::ShaderStages,
    address_space: &AddressSpace,
    config: &Config,
) -> TokenStream {
    let ty = match &typ.inner {
        TypeInner::Scalar { .. } | TypeInner::Array { .. } | TypeInner::Struct { .. } => {
            let buffer_binding_type = match address_space {
                AddressSpace::Uniform => quote! { wgpu::BufferBindingType::Uniform },
                AddressSpace::Storage { access } => {
                    let is_write = access.contains(StorageAccess::STORE);

                    if is_write {
                        quote! {wgpu::BufferBindingType::Storage { read_only: false }}
                    } else {
                        quote! {wgpu::BufferBindingType::Storage { read_only: true }}
                    }
                }
                _ => unimplemented!("rest of address spaces"),
            };

            let rust_type = rust_type(module, typ, config)
                .expect("unknown rust type (this will be handled at a later date)");

            let min_binding_size = rust_type.quote_min_binding_size();

            quote! {wgpu::BindingType::Buffer {
                ty: #buffer_binding_type,
                has_dynamic_offset: false, // TODO: add dynamic offset
                min_binding_size: #min_binding_size,
            }}
        }

        TypeInner::Image { dim, class, .. } => {
            let view_dim = match dim {
                naga::ImageDimension::D1 => quote! { wgpu::TextureViewDimension::D1 },
                naga::ImageDimension::D2 => quote! { wgpu::TextureViewDimension::D2 },
                naga::ImageDimension::D3 => quote! { wgpu::TextureViewDimension::D3 },
                naga::ImageDimension::Cube => quote! { wgpu::TextureViewDimension::Cube },
            };

            match class {
                naga::ImageClass::Sampled { kind, multi } => {
                    let sample_type = match kind {
                        naga::ScalarKind::Sint => quote! { wgpu::TextureSampleType::Sint },
                        naga::ScalarKind::Uint => quote! { wgpu::TextureSampleType::Uint },
                        naga::ScalarKind::Float => {
                            // TODO: make filterable customisable
                            quote! { wgpu::TextureSampleType::Float { filterable: true } }
                        }
                        _ => panic!("Unsupported sample type: {kind:#?}"),
                    };
                    quote! { wgpu::BindingType::Texture {
                        sample_type: #sample_type,
                        view_dimension: #view_dim,
                        multisampled: #multi,
                    }}
                }
                naga::ImageClass::Depth { multi } => {
                    quote!(wgpu::BindingType::Texture {
                        sample_type: wgpu::TextureSampleType::Depth,
                        view_dimension: #view_dim,
                        multisampled: #multi,
                    })
                }
                naga::ImageClass::Storage { format, access } => {
                    let (format_type, access_type) = storage_texture_type(format, access);

                    quote! {wgpu::BindingType::StorageTexture {
                        access: #access_type,
                        format: #format_type,
                        view_dimension: #view_dim,
                    }}
                }
            }
        }

        TypeInner::Sampler { comparison } => {
            let sampler_type = if *comparison {
                quote!(wgpu::SamplerBindingType::Comparison)
            } else {
                quote!(wgpu::SamplerBindingType::Filtering)
            };
            quote!(wgpu::BindingType::Sampler(#sampler_type))
        }

        _ => panic!("unkown bind group layout entry type"),
    };

    let visibility = quote_visibility(visibility);

    quote! {
        wgpu::BindGroupLayoutEntry {
            binding: #index,
            visibility: #visibility,
            ty: #ty,
            count: None,
        }
    }
}

fn storage_texture_type(
    format: &StorageFormat,
    access: &StorageAccess,
) -> (TokenStream, TokenStream) {
    // use debug representation of enum as ident
    let ident = identify(&format!("{format:?}"));
    let format = quote! { #ident };

    let is_read = access.contains(naga::StorageAccess::LOAD);
    let is_write = access.contains(naga::StorageAccess::STORE);
    let access = match (is_read, is_write) {
        (true, true) => quote! { wgpu::StorageTextureAccess::ReadWrite },
        (true, false) => quote! { wgpu::StorageTextureAccess::ReadOnly },
        (false, true) => quote! { wgpu::StorageTextureAccess::WriteOnly },
        _ => unreachable!(),
    };

    (format, access)
}
