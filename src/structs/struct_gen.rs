use std::collections::HashSet;

use naga::{
    proc::{Layouter, TypeLayout},
    ArraySize, Binding, Handle, Module, ShaderStage, StructMember, Type, TypeInner,
};
use proc_macro2::{Ident, Literal, TokenStream};
use quote::quote;

use crate::{
    structs::vertex_layout::build_vertex_buffer_descriptor,
    utils::{identify, naga_undecorate, rust_type_info::rust_type},
    Config,
};

// fn struct_has_dynamic_array_member(members: &[StructMember], module: &Module) -> bool {
//     members.iter().any(|m| {
//         matches!(
//             module.types[m.ty].inner,
//             TypeInner::Array {
//                 size: ArraySize::Dynamic,
//                 ..
//             }
//         )
//     })
// }

pub(crate) struct GeneratedPadding {
    pub name: Ident,
    pub size: TokenStream,
}

pub(crate) struct GeneratedField<'a> {
    pub name: Ident,
    pub member: &'a StructMember,
    pub typ: &'a Type,
    pub rust_type: TokenStream,
    pub location: Option<u32>,
    // is_dynamically_sized: bool,
}

pub(crate) enum RustStructMember<'a> {
    Field(GeneratedField<'a>),
    Padding(GeneratedPadding),
}

fn build_struct_fields(
    module: &Module,
    members: &[RustStructMember],
    is_host_shareable: bool,
) -> Vec<TokenStream> {
    let gctx = module.to_ctx();

    let mut fields = vec![];

    for member in members {
        let toks = match member {
            RustStructMember::Field(field) => {
                let GeneratedField {
                    name,
                    rust_type,
                    member,
                    typ,
                    ..
                } = field;

                let doc_comment = if is_host_shareable {
                    let offset = member.offset;
                    let size = typ.inner.size(gctx);
                    let ty_name = typ.inner.to_wgsl(&gctx);
                    // let ty_name = demangle_str(&ty_name); // TODO: demangle?
                    let doc = format!("size: {size}, offset: 0x{offset:X}, type: `{ty_name}`");

                    quote!(#[doc = #doc])
                } else {
                    quote!()
                };

                quote! {
                  #doc_comment
                  pub #name: #rust_type
                }
            }
            RustStructMember::Padding(padding) => {
                let GeneratedPadding { name, size } = padding;
                quote! { #name: [u8; const { #size }] }
            }
        };

        fields.push(toks);
    }

    fields
}

fn build_struct_new(name: &Ident, members: &[RustStructMember]) -> TokenStream {
    let mut arguments = vec![];
    let mut field_data = vec![];

    for member in members {
        match member {
            RustStructMember::Field(generated_field) => {
                let GeneratedField {
                    name, rust_type, ..
                } = generated_field;

                arguments.push(quote! {
                    #name: #rust_type
                });

                field_data.push(quote! {
                    #name
                });
            }
            RustStructMember::Padding(generated_padding) => {
                let GeneratedPadding { name, size } = generated_padding;

                field_data.push(quote! {
                    #name: [0; const { #size }]
                })
            }
        }
    }

    quote! {
        impl #name {
            pub fn new(
                #(#arguments),*
            ) -> Self {
                Self {
                    #(#field_data),*
                }
            }
        }
    }
}

fn build_main_struct(
    module: &Module,
    name_ident: &Ident,
    members: &[RustStructMember],
    layout: &TypeLayout,
    is_host_shareable: bool,
    has_dynamic_array: bool,
) -> TokenStream {
    // TODO: custom alignment maybe
    // let custom_alignment = self
    //     .options
    //     .override_struct_alignment
    //     .iter()
    //     .find_map(|struct_align| {
    //         struct_align
    //             .struct_regex
    //             .is_match(fully_qualified_name)
    //             .then_some(struct_align.alignment as u32)
    //     })
    //     .map(|align| naga::proc::Alignment::new(align))
    //     .flatten();

    let should_generate_padding = is_host_shareable;

    // TODO: custom derives maybe
    let derives = quote! {
        #[cfg_attr(debug_assertions, derive(Debug))]
        #[derive(PartialEq, Clone, Copy, Default, bytemuck::Pod, bytemuck::Zeroable)]
    };

    let alignment = Literal::usize_unsuffixed((layout.alignment * 1u32) as usize);

    let repr_c = if !has_dynamic_array {
        if should_generate_padding {
            quote!(#[repr(C, align(#alignment))])
        } else {
            quote!(#[repr(C)])
        }
    } else {
        TokenStream::new()
    };

    let fields = build_struct_fields(module, members, is_host_shareable);
    let new_impl = build_struct_new(name_ident, members);

    // TODO: assert layout?
    // let assert_layout = self.build_layout_assertion(custom_alignment);
    // we use bytemuck in a derive instead so the impl's arent necessary
    // TODO: maybe in the future we can allow for other variations aside from bytemuck but thats kinda the point of wgsl-bindgen
    // let unsafe_bytemuck_pod_impl = self.build_bytemuck_impls();

    // TODO: custom visibility maybe
    // let visibility = self.options.type_visibility.generate_quote();

    quote! {
        #[allow(non_snake_case)]
        #repr_c
        #derives
        pub struct #name_ident {
            #(#fields),*
        }

        #new_impl
    }
}

#[allow(clippy::too_many_arguments)]
fn build_struct(
    module: &Module,
    name: &str,
    members: &[RustStructMember],
    layout: &TypeLayout,
    is_host_shareable: bool,
    has_dynamic_array: bool,
    is_vertex_input: bool,
    config: &mut Config,
) -> TokenStream {
    let name_ident = identify(name);

    let struct_tokens = if !config.resolve_type_map.contains_key(name) {
        let path = identify(&config.file_name);

        config.resolve_type_map.insert(
            name.to_string(),
            quote! {
                #path::structs::#name_ident
            },
        );

        build_main_struct(
            module,
            &name_ident,
            members,
            layout,
            is_host_shareable,
            has_dynamic_array,
        )
    } else {
        TokenStream::new()
    };

    let vertex_descriptor_impl = if is_vertex_input && !config.vertex_descriptor_impl.contains(name)
    {
        config.vertex_descriptor_impl.insert(name.to_string());

        build_vertex_buffer_descriptor(config.resolve_type(name), members)
    } else {
        TokenStream::new()
    };

    quote! {
        #struct_tokens
        #vertex_descriptor_impl
    }
}

fn make_struct(
    name: &str,
    members: &[StructMember],
    layouter: &Layouter,
    t_handle: Handle<Type>,
    module: &Module,
    global_variable_types: &HashSet<Handle<Type>>,
    config: &mut Config, // options: &WgslBindgenOption,
) -> (TokenStream, bool) {
    let gctx = module.to_ctx();
    let layout = layouter[t_handle];

    let mut has_dynamic_array = false; //struct_has_dynamic_array_member(members, module);
    let is_host_shareable = global_variable_types.contains(&t_handle);

    let is_vertex_input = module
        .entry_points
        .iter()
        .filter(|e| e.stage == ShaderStage::Vertex)
        .any(|e| {
            e.function
                .arguments
                .iter()
                .filter(|a| a.binding.is_none())
                .any(|a| a.ty == t_handle)
        });

    let mut rust_struct_members = vec![];

    let mut struct_total_size: u32 = 0;

    for (index, member) in members.iter().enumerate() {
        let member_name = member.name.as_ref().unwrap();
        let name_ident = identify(member_name);

        let naga_type = &module.types[member.ty];

        has_dynamic_array |= matches!(
            naga_type.inner,
            TypeInner::Array {
                size: ArraySize::Dynamic,
                ..
            }
        );

        let rust_type = rust_type(module, naga_type, config)
            .expect("unknown rust type (this will be handled at a later date)");

        let is_dynamically_sized = rust_type.size.is_none();

        if is_dynamically_sized && index != members.len() - 1 {
            panic!("only the last field of a struct can be a runtime-sized array");
        }

        // check if we need padding bytes between the current member and the next member
        let padding = if is_dynamically_sized || !is_host_shareable {
            None
        } else {
            // let current_offset = member.offset;
            // // use the next member to calculate what the size of the current member is
            // let next_offset = if index + 1 < members.len() {
            //     members[index + 1].offset
            // } else {
            //     // struct_total_size
            //     layout.size
            // };

            // the only type that does not have a size is a dynamic array,
            // and due to the check above that type should never show up here
            let member_size = rust_type.size.expect("dynamic array") as u32;
            let aligned_member_size = rust_type.alignment.round_up(member_size);

            struct_total_size += aligned_member_size;

            // println!("{member_name}, {}", rust_type.size.unwrap());

            // if there is a difference in the aligned size and the normal size then the member is unaligned
            // so we need to add the difference as padding
            let has_padding = if aligned_member_size - member_size > 0 {
                // padding
                let pad_name = identify(&format!("_pad_{}", member_name));
                let size = Literal::usize_suffixed(aligned_member_size as usize);
                let pad_size = quote! { #size - core::mem::size_of::<#rust_type>() };

                let padding = GeneratedPadding {
                    name: pad_name,
                    size: pad_size,
                };

                Some(padding)
            } else {
                let final_padding = layout.size - struct_total_size;
                // if there is any padding needed between the final member and the end of the struct, this will add it
                if final_padding > 0 && index + 1 == members.len() {
                    let pad_name = identify("_pad_end");
                    let size = Literal::usize_suffixed(final_padding as usize);
                    let pad_size = quote! { #size };

                    let padding = GeneratedPadding {
                        name: pad_name,
                        size: pad_size,
                    };

                    Some(padding)
                } else {
                    None
                }
            };

            has_padding
            // let rust_type = &rust_type;

            // let member_size = next_offset - current_offset;

            // println!("{member_name}, member_size {member_size}, current_offset {current_offset}, next_offset {next_offset}, aligned {:?}", rust_type.alignment.round_up(member_size as u32));

            // // dbg!(rust_type.alignment.round_up(member_size as u32));

            // match rust_type.alignment.round_up(member_size as u32) as usize {
            //     // rust_type_size if member_size == rust_type_size => None,
            //     aligned => {
            //     }
            // }
        };

        // TODO: custom padding field members maybe
        // let is_current_field_padding = options
        //     .custom_padding_field_regexps
        //     .iter()
        //     .any(|pad_expr| pad_expr.is_match(&member_name));

        // both padding field and built-in fields are handled in the same way
        // skip builtins like @builtin(vertex_index)
        let entry = if matches!(member.binding, Some(Binding::BuiltIn(_))) {
            let size = naga_type.inner.size(gctx) as usize;
            let pad_size = quote! { #size };

            RustStructMember::Padding(GeneratedPadding {
                name: name_ident,
                size: pad_size,
            })
        } else {
            let location = match member.binding.as_ref() {
                Some(Binding::Location { location, .. }) => Some(*location),
                _ => None,
            };
            RustStructMember::Field(GeneratedField {
                name: name_ident.clone(),
                member,
                typ: naga_type,
                rust_type: rust_type.tokens,
                location, // is_dynamically_sized,
            })
        };

        // add current member first as the padding goes between the current member and next member
        rust_struct_members.push(entry);

        // insert the padding
        if let Some(padding) = padding {
            rust_struct_members.push(RustStructMember::Padding(padding));
        }
    }

    (
        build_struct(
            module,
            name,
            &rust_struct_members,
            &layout,
            is_host_shareable,
            has_dynamic_array,
            is_vertex_input,
            config,
        ),
        is_vertex_input,
    )
}

pub fn make_structs(module: &Module, config: &mut Config) -> (TokenStream, Vec<String>) {
    let mut layouter = Layouter::default();
    layouter.update(module.to_ctx()).unwrap();

    let mut tokens = TokenStream::new();

    let mut vertex_inputs = vec![];

    // im not exactly sure what this does
    let mut global_variable_types = HashSet::new();
    for g in module.global_variables.iter() {
        add_types_recursive(&mut global_variable_types, module, g.1.ty);
    }

    // global_variable_types.iter().for_each(|t| {
    //     dbg!(module.types.get_handle(*t));
    // });

    for (handle, typ) in module.types.iter().filter(|(handle, _)| {
        // TODO: filter some types?
        true
    }) {
        if let TypeInner::Struct { members, .. } = &typ.inner {
            let name = naga_undecorate(typ.name.as_ref().expect("struct with name"));

            // let ident_name = identify(name);

            let (struct_tokens, is_vertex_input) = make_struct(
                name,
                members,
                &layouter,
                handle,
                module,
                &global_variable_types,
                config,
            );

            tokens.extend(struct_tokens);

            if is_vertex_input {
                vertex_inputs.push(name.to_string())
            }
        } else {
            continue;
        }
    }

    let out = quote! {
        pub mod structs {
            #[allow(unused_imports)]
            pub use super::*;

            #tokens
        }
    };

    (out, vertex_inputs)
}

fn add_types_recursive(types: &mut HashSet<Handle<Type>>, module: &Module, ty: Handle<Type>) {
    types.insert(ty);

    match &module.types[ty].inner {
        TypeInner::Pointer { base, .. } => add_types_recursive(types, module, *base),
        TypeInner::Array { base, .. } => add_types_recursive(types, module, *base),
        TypeInner::Struct { members, .. } => {
            for member in members {
                add_types_recursive(types, module, member.ty);
            }
        }
        TypeInner::BindingArray { base, .. } => add_types_recursive(types, module, *base),
        _ => (),
    }
}
