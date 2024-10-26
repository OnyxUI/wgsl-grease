// use naga::{
//     ArraySize, Binding, ImageClass, ImageDimension, Module, ScalarKind, TypeInner, VectorSize,
// };

// #[derive(Clone, Copy, Debug)]
// #[non_exhaustive]
// pub enum Scalar {
//     Bool,
//     /// f32 (4) or f64 (8)
//     Float(u8),
//     /// u32 or u64
//     UInt(u8),
//     /// i32 or i64
//     Int(u8),
// }

// impl Scalar {
//     #[inline]
//     pub fn width(&self) -> u8 {
//         match self {
//             Scalar::Bool => 1,
//             Scalar::Float(width) | Scalar::UInt(width) | Scalar::Int(width) => *width,
//         }
//     }
// }

// #[derive(Clone, Copy, Debug)]
// #[non_exhaustive]
// pub enum MatScalar {
//     /// f32 or f64
//     Float(u8),
// }

// impl MatScalar {
//     #[inline]
//     pub fn width(&self) -> u8 {
//         match self {
//             MatScalar::Float(width) => *width,
//         }
//     }
// }

// #[derive(Clone, Debug)]
// #[non_exhaustive]
// pub struct StructMember {
//     pub name: Option<String>,
//     pub ty: WgslType,
//     pub binding: Option<Binding>,
//     pub offset: u32,
// }

// #[derive(Clone, Debug)]
// #[non_exhaustive]
// pub enum WgslType {
//     Scalar(Scalar),
//     Vector {
//         size: VectorSize,
//         scalar: Scalar,
//     },
//     Matrix {
//         columns: VectorSize,
//         rows: VectorSize,
//         scalar: MatScalar,
//     },
//     Array {
//         base: Box<WgslType>,
//         size: ArraySize,
//         stride: u32,
//     },
//     Struct {
//         members: Vec<StructMember>,
//         span: u32,
//     },
//     Image {
//         dim: ImageDimension,
//         arrayed: bool,
//         class: ImageClass,
//     },
//     Sampler {
//         comparison: bool,
//     },
//     Atomic(Scalar),
// }

// fn scalar_to_our_scalar(scalar: &naga::Scalar) -> Scalar {
//     match scalar {
//         naga::Scalar {
//             kind: ScalarKind::Bool,
//             ..
//         } => Scalar::Bool,
//         naga::Scalar {
//             width,
//             kind: ScalarKind::AbstractFloat | ScalarKind::Float,
//         } => Scalar::Float(*width),
//         naga::Scalar {
//             width,
//             kind: ScalarKind::Sint,
//         } => Scalar::Int(*width),
//         naga::Scalar {
//             width,
//             kind: ScalarKind::Uint,
//         } => Scalar::UInt(*width),
//         _ => unimplemented!("scalar type"),
//     }
// }
// fn scalar_to_mat_scalar(scalar: &naga::Scalar) -> MatScalar {
//     match scalar {
//         naga::Scalar {
//             width,
//             kind: ScalarKind::AbstractFloat | ScalarKind::Float,
//         } => MatScalar::Float(*width),
//         _ => unimplemented!("mat scalar type"),
//     }
// }

// impl WgslType {
//     pub fn from_naga(value: &TypeInner, module: &Module) -> Option<Self> {
//         Some(match value {
//             TypeInner::Scalar(scalar) => Self::Scalar(scalar_to_our_scalar(scalar)),
//             TypeInner::Vector { size, scalar } => Self::Vector {
//                 size: *size,
//                 scalar: scalar_to_our_scalar(scalar),
//             },
//             TypeInner::Matrix {
//                 columns,
//                 rows,
//                 scalar,
//             } => Self::Matrix {
//                 columns: *columns,
//                 rows: *rows,
//                 scalar: scalar_to_mat_scalar(scalar),
//             },
//             TypeInner::Array { base, size, stride } => Self::Array {
//                 base: Box::new(WgslType::from_naga(
//                     &module
//                         .types
//                         .get_handle(*base)
//                         .expect("todo array type error")
//                         .inner,
//                     module,
//                 )?),
//                 size: *size,
//                 stride: *stride,
//             },
//             TypeInner::Struct { members, span } => Self::Struct {
//                 members: members
//                     .iter()
//                     .map(|m| {
//                         Some(StructMember {
//                             name: m.name.clone(),
//                             ty: WgslType::from_naga(
//                                 &module
//                                     .types
//                                     .get_handle(m.ty)
//                                     .expect("todo array type error")
//                                     .inner,
//                                 module,
//                             )?,
//                             binding: m.binding.clone(),
//                             offset: m.offset,
//                         })
//                     })
//                     .collect::<Option<Vec<_>>>()?,
//                 span: *span,
//             },
//             TypeInner::Image {
//                 dim,
//                 arrayed,
//                 class,
//             } => Self::Image {
//                 dim: *dim,
//                 arrayed: *arrayed,
//                 class: *class,
//             },
//             TypeInner::Sampler { comparison } => Self::Sampler {
//                 comparison: *comparison,
//             },
//             TypeInner::Atomic(scalar) => Self::Atomic(scalar_to_our_scalar(scalar)),

//             _ => return None,
//         })
//     }
// }
