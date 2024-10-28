use naga::{Module, ScalarKind, Type, TypeInner, VectorSize};
use naga_oil::compose::tokenizer::Token;
use proc_macro2::TokenStream;
use quote::quote;

use crate::{structs::struct_gen::GeneratedField, utils::identify};

use super::struct_gen::RustStructMember;

// from wgsl-bindgen
pub fn quote_vertex_format(ty: &Type) -> TokenStream {
    // Not all wgsl types work as vertex attributes in wgpu.
    match &ty.inner {
        TypeInner::Scalar(scalar) => match (scalar.kind, scalar.width) {
            (ScalarKind::Sint, 4) => quote! { wgpu::VertexFormat::Sint32 },
            (ScalarKind::Uint, 4) => quote! { wgpu::VertexFormat::Uint32 },
            (ScalarKind::Float, 4) => quote! { wgpu::VertexFormat::Float32 },
            (ScalarKind::Float, 8) => quote! { wgpu::VertexFormat::Float64 },
            s => todo!("scalar vertex format {s:?}"),
        },
        TypeInner::Vector { size, scalar } => match size {
            VectorSize::Bi => match (scalar.kind, scalar.width) {
                (ScalarKind::Sint, 1) => quote! { wgpu::VertexFormat::Sint8x2 },
                (ScalarKind::Uint, 1) => quote! { wgpu::VertexFormat::Uint8x2 },
                (ScalarKind::Sint, 2) => quote! { wgpu::VertexFormat::Sint16x2 },
                (ScalarKind::Uint, 2) => quote! { wgpu::VertexFormat::Uint16x2 },
                (ScalarKind::Uint, 4) => quote! { wgpu::VertexFormat::Uint32x2 },
                (ScalarKind::Sint, 4) => quote! { wgpu::VertexFormat::Sint32x2 },
                (ScalarKind::Float, 4) => quote! { wgpu::VertexFormat::Float32x2 },
                (ScalarKind::Float, 8) => quote! { wgpu::VertexFormat::Float64x2 },
                s => todo!("vec2 vertex format {s:?}"),
            },
            VectorSize::Tri => match (scalar.kind, scalar.width) {
                (ScalarKind::Uint, 4) => quote! { wgpu::VertexFormat::Uint32x3 },
                (ScalarKind::Sint, 4) => quote! { wgpu::VertexFormat::Sint32x3 },
                (ScalarKind::Float, 4) => quote! { wgpu::VertexFormat::Float32x3 },
                (ScalarKind::Float, 8) => quote! { wgpu::VertexFormat::Float64x3 },
                s => todo!("vec3 vertex format {s:?}"),
            },
            VectorSize::Quad => match (scalar.kind, scalar.width) {
                (ScalarKind::Sint, 1) => quote! { wgpu::VertexFormat::Sint8x4 },
                (ScalarKind::Uint, 1) => quote! { wgpu::VertexFormat::Uint8x4 },
                (ScalarKind::Sint, 2) => quote! { wgpu::VertexFormat::Sint16x4 },
                (ScalarKind::Uint, 2) => quote! { wgpu::VertexFormat::Uint16x4 },
                (ScalarKind::Uint, 4) => quote! { wgpu::VertexFormat::Uint32x4 },
                (ScalarKind::Sint, 4) => quote! { wgpu::VertexFormat::Sint32x4 },
                (ScalarKind::Float, 4) => quote! { wgpu::VertexFormat::Float32x4 },
                (ScalarKind::Float, 8) => quote! { wgpu::VertexFormat::Float64x4 },
                s => todo!("vec4 vertex format {s:?}"),
            },
        },
        s => todo!("other vertex format {s:?}"), // are these types even valid as attributes?
    }
}
pub fn build_vertex_buffer_descriptor(
    path: TokenStream,
    members: &[RustStructMember],
) -> TokenStream {
    // let name = identify(name);

    let mut attributes = vec![];

    for member in members {
        match member {
            RustStructMember::Field(generated_field) => {
                let GeneratedField {
                    name,
                    typ,
                    location,
                    ..
                } = generated_field;

                let location = location.expect("location missing in vertex attribute");

                let format = quote_vertex_format(typ);

                attributes.push(quote! {
                    wgpu::VertexAttribute {
                        offset: ::core::mem::offset_of!(Self, #name) as wgpu::BufferAddress,
                        shader_location: #location,
                        format: #format,
                    }
                });
            }
            RustStructMember::Padding(..) => {
                unreachable!("padding in vertex input struct")
            }
        }
    }

    quote! {
        impl #path {
            pub fn vertex_desc(step_mode: wgpu::VertexStepMode) -> wgpu::VertexBufferLayout<'static> {
                wgpu::VertexBufferLayout {
                    array_stride: std::mem::size_of::<Self>() as wgpu::BufferAddress,
                    step_mode,
                    attributes: &[
                        #(#attributes),*
                    ],
                }
            }
        }
    }
}
