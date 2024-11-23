// HUGE thanks to wgsl-bindgen for the majority of this file

use naga::{
    proc::{Alignment, Layouter},
    ArraySize, Scalar, ScalarKind, TypeInner,
};
use proc_macro2::TokenStream;
use quote::{quote, ToTokens};

use crate::Config;

use super::{identify, naga_undecorate};

// use crate::type_map::{};

#[derive(Debug)]
pub struct RustTypeInfo {
    pub tokens: TokenStream,
    // size in bytes, if none then it is a runtime sized array
    pub size: Option<usize>,
    pub alignment: Alignment,
}

impl RustTypeInfo {
    pub fn new(tokens: TokenStream, alignment: Alignment) -> Self {
        Self {
            tokens,
            size: None,
            alignment,
        }
    }

    pub fn new_size(tokens: TokenStream, size: usize, alignment: Alignment) -> Self {
        Self {
            tokens,
            size: Some(size),
            alignment,
        }
    }

    pub fn aligned_size(&self) -> Option<usize> {
        let size = self.size? as u32;
        Some(self.alignment.round_up(size) as usize)
    }

    pub fn quote_min_binding_size(&self) -> TokenStream {
        if self.size.is_none() {
            quote!(None)
        } else {
            let ty = quote! { #self };
            quote!(std::num::NonZeroU64::new(std::mem::size_of::<#ty>() as _))
        }
    }
}

impl ToTokens for RustTypeInfo {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        tokens.extend(self.tokens.clone())
    }
}

/// Get the array stride and padding in bytes
fn get_stride_and_padding(
    alignment: naga::proc::Alignment,
    size: naga::VectorSize,
    width: u8,
) -> (u32, u32) {
    let width = width as u32;
    let rows = size as u32;
    let used_bytes = rows * width;
    let total_bytes = alignment.round_up(used_bytes);
    let padding_bytes = total_bytes - used_bytes;

    println!("width {width}, rows {rows}, used_bytes {used_bytes}, total_bytes {total_bytes}, padding_bytes {padding_bytes} ");

    // NOTE: bytemuch only strategy
    (total_bytes, padding_bytes)
}

fn rust_scalar_type(scalar: Scalar, alignment: Alignment) -> Option<RustTypeInfo> {
    Some(match (scalar.kind, scalar.width) {
        (ScalarKind::Sint, 1) => RustTypeInfo::new_size(quote!(i8), 1, alignment),
        (ScalarKind::Uint, 1) => RustTypeInfo::new_size(quote!(u8), 1, alignment),
        (ScalarKind::Sint, 2) => RustTypeInfo::new_size(quote!(i16), 2, alignment),
        (ScalarKind::Uint, 2) => RustTypeInfo::new_size(quote!(u16), 2, alignment),
        (ScalarKind::Sint, 4) => RustTypeInfo::new_size(quote!(i32), 4, alignment),
        (ScalarKind::Uint, 4) => RustTypeInfo::new_size(quote!(u32), 4, alignment),
        (ScalarKind::Float, 4) => RustTypeInfo::new_size(quote!(f32), 4, alignment),
        (ScalarKind::Float, 8) => RustTypeInfo::new_size(quote!(f64), 8, alignment),

        (ScalarKind::Bool, 1) => RustTypeInfo::new_size(quote!(bool), 1, alignment),
        _ => panic!("unknown scalar in `rust_type`"),
    })
}

pub fn rust_type(module: &naga::Module, ty: &naga::Type, config: &Config) -> Option<RustTypeInfo> {
    let t_handle = module.types.get(ty).unwrap();
    let mut layouter = Layouter::default();
    layouter.update(module.to_ctx()).unwrap();

    let type_layout = layouter[t_handle];

    let alignment = type_layout.alignment;

    // TODO: here is where passing it to some user implementation could be an extra feature
    Some(match &ty.inner {
        TypeInner::Scalar(scalar) => rust_scalar_type(*scalar, alignment)?,
        TypeInner::Vector { size, scalar } => {
            let (stride, padding) = get_stride_and_padding(alignment, *size, scalar.width);
            let inner_type = rust_scalar_type(*scalar, alignment)?.tokens;
            let len = ((stride - padding) / scalar.width as u32) as usize;

            RustTypeInfo::new_size(
                quote! { [#inner_type; #len] },
                (stride - padding) as usize,
                alignment,
            )
        }
        TypeInner::Matrix {
            columns,
            rows,
            scalar,
        } => {
            let inner_type = rust_scalar_type(*scalar, alignment)?.tokens;
            let (col_array_stride, _) = get_stride_and_padding(alignment, *rows, scalar.width);
            let size = col_array_stride * (*columns as u32);

            // let rows = Index::from((col_array_stride / scalar.width as u32) as usize);
            let cols = *columns as usize;
            let rows = (col_array_stride / scalar.width as u32) as usize;
            RustTypeInfo::new_size(
                quote! { [[#inner_type; #rows]; #cols] },
                size as usize,
                alignment,
            )
        }
        TypeInner::Atomic(scalar) => rust_scalar_type(*scalar, alignment)?,
        TypeInner::Array {
            base,
            size: ArraySize::Constant(size),
            stride,
        } => {
            let inner_ty = rust_type(module, &module.types[*base], config)?.tokens;

            let count = size.get() as usize;

            RustTypeInfo::new_size(quote! { [#inner_ty; #count] }, *stride as usize, alignment)
        }
        TypeInner::Array {
            base,
            size: naga::ArraySize::Dynamic,
            ..
        } => {
            // panic!("Runtime-sized arrays can only be used in variable declarations or as the last field of a struct.");
            let element_type = rust_type(module, &module.types[*base], config)?.tokens;

            RustTypeInfo::new(quote! { [#element_type; N] }, alignment)
        }
        TypeInner::Struct { members, span: _ } => {
            let name = naga_undecorate(ty.name.as_deref().unwrap());

            let name = config.resolve_type(name);

            // let name = identify(name_str); //demangle_and_fully_qualify(name_str, invoking_entry_module); // TODO: demangle name string?

            let size = type_layout.size as usize;

            // custom map struct
            let mut mapped_type = RustTypeInfo::new_size(quote! { #name }, size, alignment);

            // check if the last member is a runtime sized array
            if let Some(last) = members.last() {
                if let naga::TypeInner::Array {
                    size: naga::ArraySize::Dynamic,
                    ..
                } = &module.types[last.ty].inner
                {
                    mapped_type.size = None;
                }
            }

            mapped_type
        }
        // TODO: rest of types
        _ => return None,
    })
}
