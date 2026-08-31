//! <https://www.w3.org/TR/WGSL/#memory-layouts>
//! .

use base_db::Intern as _;
use hir_def::signature::LocalFieldId;
use la_arena::ArenaMap;
use wgsl_types::syntax::AddressSpace;

use crate::{
    db::HirDatabase,
    ty::{ArraySize, ArrayType, ScalarType, Type, TypeKind, VecSize, VectorType},
};

type Bytes = u32;

const fn round_up(
    multiple: Bytes,
    num: Bytes,
) -> Bytes {
    num.div_ceil(multiple) * multiple
}

impl ArrayType {
    pub fn stride(
        &self,
        address_space: AddressSpace,
        db: &dyn HirDatabase,
    ) -> Option<Bytes> {
        let stride = round_up(
            self.inner.align_of(address_space, db)?,
            self.inner.size_of(address_space, db)?,
        );
        if address_space == AddressSpace::Uniform {
            Some(round_up(16, stride))
        } else {
            Some(stride)
        }
    }
}

impl Type {
    #[expect(clippy::doc_paragraphs_missing_punctuation, reason = "false positive")]
    /// <https://www.w3.org/TR/WGSL/#alignof>
    pub fn align_of(
        self,
        address_space: AddressSpace,
        db: &dyn HirDatabase,
    ) -> Option<Bytes> {
        #[expect(
            clippy::match_same_arms,
            reason = "a match arm corresponds to a table row in the specification"
        )]
        match self.kind(db) {
            // <https://www.w3.org/TR/WGSL/#why-is-bool-4-bytes>
            TypeKind::Scalar(ScalarType::Bool) => Some(4),
            TypeKind::Scalar(ScalarType::I32 | ScalarType::U32 | ScalarType::F32) => Some(4),
            // SHADER_INT64
            TypeKind::Scalar(ScalarType::I64 | ScalarType::U64) => Some(8),
            TypeKind::Scalar(ScalarType::F16) => Some(2),
            TypeKind::Atomic(_) => Some(4),
            TypeKind::Vector(VectorType {
                size: VecSize::Two,
                component_type,
            }) if matches!(
                component_type.kind(db),
                TypeKind::Scalar(
                    ScalarType::Bool | ScalarType::I32 | ScalarType::U32 | ScalarType::F32
                )
            ) =>
            {
                Some(8)
            },
            TypeKind::Vector(VectorType {
                size: VecSize::Two,
                component_type,
            }) if matches!(component_type.kind(db), TypeKind::Scalar(ScalarType::F16)) => Some(4),
            TypeKind::Vector(VectorType {
                size: VecSize::Three,
                component_type,
            }) if matches!(
                component_type.kind(db),
                TypeKind::Scalar(
                    ScalarType::Bool | ScalarType::I32 | ScalarType::U32 | ScalarType::F32
                )
            ) =>
            {
                Some(16)
            },
            TypeKind::Vector(VectorType {
                size: VecSize::Three,
                component_type,
            }) if matches!(component_type.kind(db), TypeKind::Scalar(ScalarType::F16)) => Some(8),
            TypeKind::Vector(VectorType {
                size: VecSize::Four,
                component_type,
            }) if matches!(
                component_type.kind(db),
                TypeKind::Scalar(
                    ScalarType::Bool | ScalarType::I32 | ScalarType::U32 | ScalarType::F32
                )
            ) =>
            {
                Some(16)
            },
            TypeKind::Vector(VectorType {
                size: VecSize::Four,
                component_type,
            }) if matches!(component_type.kind(db), TypeKind::Scalar(ScalarType::F16)) => Some(8),
            TypeKind::Matrix(matrix_type) => TypeKind::Vector(VectorType {
                size: matrix_type.rows,
                component_type: matrix_type.inner,
            })
            .intern(db)
            .align_of(address_space, db),
            TypeKind::Struct(r#struct) => {
                let fields = &db.field_types(r#struct).0;
                let (align, _) =
                    struct_member_layout(fields, db, AddressSpace::Storage, |_, _, _| {})?;
                Some(if address_space == AddressSpace::Uniform {
                    round_up(16, align)
                } else {
                    align
                })
            },
            TypeKind::Array(array) => {
                let inner_align = array.inner.align_of(address_space, db)?;
                Some(if address_space == AddressSpace::Uniform {
                    round_up(16, inner_align)
                } else {
                    inner_align
                })
            },
            TypeKind::Error
            | TypeKind::Scalar(ScalarType::AbstractFloat | ScalarType::AbstractInt)
            | TypeKind::Vector(_)
            | TypeKind::SwizzleView(_)
            | TypeKind::BuiltinStruct(_)
            | TypeKind::Texture(_)
            | TypeKind::Sampler(_)
            | TypeKind::AccelerationStructure(_)
            | TypeKind::Reference(_)
            | TypeKind::Pointer(_) => None,
        }
    }

    #[expect(clippy::doc_paragraphs_missing_punctuation, reason = "false positive")]
    /// <https://www.w3.org/TR/WGSL/#sizeof>
    ///
    /// # Panics
    ///
    /// Panics if the size of the array exceeds u32.
    pub fn size_of(
        self,
        address_space: AddressSpace,
        db: &dyn HirDatabase,
    ) -> Option<Bytes> {
        #[expect(
            clippy::match_same_arms,
            reason = "a match arm corresponds to a table row in the specification"
        )]
        match self.kind(db) {
            TypeKind::Scalar(ScalarType::Bool) => Some(4),
            TypeKind::Scalar(ScalarType::I32 | ScalarType::U32 | ScalarType::F32) => Some(4),
            // SHADER_INT64
            TypeKind::Scalar(ScalarType::I64 | ScalarType::U64) => Some(8),
            TypeKind::Scalar(ScalarType::F16) => Some(2),
            TypeKind::Atomic(_) => Some(4),
            TypeKind::Vector(VectorType {
                size: VecSize::Two,
                component_type,
            }) if matches!(
                component_type.kind(db),
                TypeKind::Scalar(
                    ScalarType::Bool | ScalarType::I32 | ScalarType::U32 | ScalarType::F32
                )
            ) =>
            {
                Some(8)
            },
            TypeKind::Vector(VectorType {
                size: VecSize::Two,
                component_type,
            }) if matches!(component_type.kind(db), TypeKind::Scalar(ScalarType::F16)) => Some(4),
            TypeKind::Vector(VectorType {
                size: VecSize::Three,
                component_type,
            }) if matches!(
                component_type.kind(db),
                TypeKind::Scalar(
                    ScalarType::Bool | ScalarType::I32 | ScalarType::U32 | ScalarType::F32
                )
            ) =>
            {
                Some(12)
            },
            TypeKind::Vector(VectorType {
                size: VecSize::Four,
                component_type,
            }) if matches!(component_type.kind(db), TypeKind::Scalar(ScalarType::F16)) => Some(6),
            TypeKind::Vector(VectorType {
                size: VecSize::Four,
                component_type,
            }) if matches!(
                component_type.kind(db),
                TypeKind::Scalar(
                    ScalarType::Bool | ScalarType::I32 | ScalarType::U32 | ScalarType::F32
                )
            ) =>
            {
                Some(16)
            },
            TypeKind::Vector(VectorType {
                size: VecSize::Three,
                component_type,
            }) if matches!(component_type.kind(db), TypeKind::Scalar(ScalarType::F16)) => Some(8),
            TypeKind::Matrix(matrix_type) => TypeKind::Vector(VectorType {
                size: matrix_type.rows,
                component_type: matrix_type.inner,
            })
            .intern(db)
            .size_of(address_space, db),
            TypeKind::Struct(r#struct) => {
                let fields = &db.field_types(r#struct).0;
                let (_, size) =
                    struct_member_layout(fields, db, AddressSpace::Storage, |_, _, _| {})?;
                Some(size)
            },
            TypeKind::Array(array) => match array.size {
                ArraySize::Fixed(size) => {
                    let stride = array.stride(address_space, db)?;
                    Some(size.unwrap_left().get().checked_mul(stride).unwrap())
                },
                ArraySize::Dynamic => None,
            },
            TypeKind::Error
            | TypeKind::Scalar(ScalarType::AbstractFloat | ScalarType::AbstractInt)
            | TypeKind::BuiltinStruct(_)
            | TypeKind::Vector(_)
            | TypeKind::SwizzleView(_)
            | TypeKind::Texture(_)
            | TypeKind::Sampler(_)
            | TypeKind::AccelerationStructure(_)
            | TypeKind::Reference(_)
            | TypeKind::Pointer(_) => None,
        }
    }
}

pub struct FieldLayout {
    pub offset: Bytes,
    pub align: Bytes,
    pub size: Bytes,
}

/// Returns the (align, size) of the struct, and calls `on_field` for every field.
pub fn struct_member_layout<Result, Function>(
    fields: &ArenaMap<LocalFieldId, Type>,
    db: &dyn HirDatabase,
    address_space: AddressSpace,
    mut on_field: Function,
) -> Option<(Bytes, Bytes)>
where
    Function: FnMut(LocalFieldId, Type, FieldLayout) -> Result,
{
    let mut struct_align = Bytes::MIN;

    let mut offset = 0;
    let mut last_member_size = None;

    for (field_id, &field) in fields.iter() {
        // TODO: handle @align and @size
        // See: https://github.com/wgsl-analyzer/wgsl-analyzer/issues/678
        let custom_align = None;
        let custom_size = None;

        let align = custom_align.or_else(|| field.align_of(address_space, db))?;
        let size = custom_size.or_else(|| field.size_of(address_space, db))?;

        struct_align = struct_align.max(align);

        on_field(
            field_id,
            field,
            FieldLayout {
                offset,
                align,
                size,
            },
        );

        let new_offset = round_up(align, offset + size);
        last_member_size = Some(size);
        offset = new_offset;
    }

    let just_past_last_member = offset + last_member_size?;
    let struct_size = round_up(struct_align, just_past_last_member);
    let struct_align = if address_space == AddressSpace::Uniform {
        round_up(16, struct_align)
    } else {
        struct_align
    };
    Some((struct_align, struct_size))
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    #[expect(
        clippy::decimal_literal_representation,
        reason = "literal is more clear"
    )]
    fn round_up_is_correct() {
        assert_eq!(round_up(16, 10), 16);
        assert_eq!(round_up(16, 16), 16);
        assert_eq!(round_up(32, 17), 32);
        assert_eq!(round_up(32, 35), 64);
        assert_eq!(round_up(32, 102), 128);
    }
}
