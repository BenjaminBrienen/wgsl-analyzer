use std::fmt;

use itertools::Itertools as _;
use smallvec::{SmallVec, smallvec};
use wgsl_types::syntax::{AccessMode, AddressSpace};

use crate::{
    db::HirDatabase,
    ty::{ArrayType, Type, TypeKind},
};

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub enum Scope {
    Function,
    Module,
}

impl fmt::Display for Scope {
    fn fmt(
        &self,
        formatter: &mut fmt::Formatter<'_>,
    ) -> fmt::Result {
        match self {
            Self::Function => write!(formatter, "function"),
            Self::Module => write!(formatter, "module"),
        }
    }
}

/// Errors which are unfulfilled expectations.
pub enum AddressSpaceError {
    AccessMode(SmallVec<[AccessMode; 2]>),
    Scope(Scope),
    Constructible,
    HostShareable,
    /// Plain type, excluding runtime-sized arrays.
    WorkgroupCompatible,
    HandleOrTexture,
    TaskPayloadCompatible,
}

impl fmt::Display for AddressSpaceError {
    fn fmt(
        &self,
        formatter: &mut fmt::Formatter<'_>,
    ) -> fmt::Result {
        match self {
            Self::AccessMode(mode) => match mode.as_slice() {
                &[mode] => write!(formatter, "expected {mode} access mode"),
                &[mode1, mode2] => write!(formatter, "expected {mode1} or {mode2} access mode"),
                other => write!(
                    formatter,
                    "expected {} access mode",
                    other.iter().format(", ")
                ),
            },
            Self::Scope(scope) => {
                write!(formatter, "address space is only valid in {scope}-scope")
            },
            Self::Constructible => formatter.write_str("type is not constructible"),
            Self::HostShareable => formatter.write_str("type is not host-shareable"),
            Self::WorkgroupCompatible => formatter.write_str(""),
            Self::HandleOrTexture => {
                formatter.write_str("address space is only valid for handle or texture types")
            },
            Self::TaskPayloadCompatible => {
                formatter.write_str("type is not compatible with `task_payload` address space")
            },
        }
    }
}

#[expect(clippy::cognitive_complexity, reason = "TODO")]
#[expect(clippy::too_many_lines, reason = "TODO")]
pub fn validate_address_space<DiagnosticBuilder>(
    address_space: AddressSpace,
    access_mode: AccessMode,
    scope: Scope,
    r#type: Type,
    db: &dyn HirDatabase,
    mut diagnostic_builder: DiagnosticBuilder,
) where
    DiagnosticBuilder: FnMut(AddressSpaceError),
{
    // We only care about the inner type here
    let unref = r#type.unref(db);
    match address_space {
        AddressSpace::Function => {
            if !matches!(scope, Scope::Function) {
                diagnostic_builder(AddressSpaceError::Scope(Scope::Function));
            }
            if !matches!(access_mode, AccessMode::ReadWrite) {
                diagnostic_builder(AddressSpaceError::AccessMode(smallvec![
                    AccessMode::ReadWrite
                ]));
            }
            if !unref.is_error(db) && !unref.is_constructible(db) {
                diagnostic_builder(AddressSpaceError::Constructible);
            }
        },
        AddressSpace::Private => {
            if !matches!(scope, Scope::Module) {
                diagnostic_builder(AddressSpaceError::Scope(Scope::Module));
            }
            if !matches!(access_mode, AccessMode::ReadWrite) {
                diagnostic_builder(AddressSpaceError::AccessMode(smallvec![
                    AccessMode::ReadWrite
                ]));
            }
            if !unref.is_error(db) && !unref.is_constructible(db) {
                diagnostic_builder(AddressSpaceError::Constructible);
            }
        },
        AddressSpace::Workgroup => {
            if !matches!(scope, Scope::Module) {
                diagnostic_builder(AddressSpaceError::Scope(Scope::Module));
            }
            if !matches!(access_mode, AccessMode::ReadWrite) {
                diagnostic_builder(AddressSpaceError::AccessMode(smallvec![
                    AccessMode::ReadWrite
                ]));
            }
            if !unref.is_error(db)
                && (!unref.is_plain(db) || unref.contains_runtime_sized_array(db))
            {
                diagnostic_builder(AddressSpaceError::WorkgroupCompatible);
            }
        },
        AddressSpace::Uniform => {
            if !matches!(scope, Scope::Module) {
                diagnostic_builder(AddressSpaceError::Scope(Scope::Module));
            }
            if !matches!(access_mode, AccessMode::Read) {
                diagnostic_builder(AddressSpaceError::AccessMode(smallvec![
                    AccessMode::ReadWrite
                ]));
            }
            if !unref.is_error(db) && !unref.is_host_shareable(db) {
                diagnostic_builder(AddressSpaceError::HostShareable);
            }
            if !unref.is_error(db) && !unref.is_constructible(db) {
                diagnostic_builder(AddressSpaceError::Constructible);
            }
        },
        AddressSpace::Storage => {
            if !matches!(scope, Scope::Module) {
                diagnostic_builder(AddressSpaceError::Scope(Scope::Module));
            }
            if !matches!(access_mode, AccessMode::ReadWrite | AccessMode::Read) {
                diagnostic_builder(AddressSpaceError::AccessMode(smallvec![
                    AccessMode::ReadWrite
                ]));
            }
            if !unref.is_error(db) && !unref.is_host_shareable(db) {
                diagnostic_builder(AddressSpaceError::HostShareable);
            }
        },
        AddressSpace::Handle => {
            if !matches!(scope, Scope::Module) {
                diagnostic_builder(AddressSpaceError::Scope(Scope::Module));
            }
            if !matches!(access_mode, AccessMode::Read) {
                diagnostic_builder(AddressSpaceError::AccessMode(smallvec![
                    AccessMode::ReadWrite
                ]));
            }
            match unref.kind(db) {
                // optimistic about using errors
                TypeKind::Error
                | TypeKind::Sampler(_)
                | TypeKind::Texture(_)
                | TypeKind::Array(ArrayType {
                    binding_array: true,
                    inner: _,
                    size: _,
                }) => {},
                TypeKind::Scalar(_)
                | TypeKind::Atomic(_)
                | TypeKind::Vector(_)
                | TypeKind::SwizzleView(_)
                | TypeKind::Matrix(_)
                | TypeKind::Struct(_)
                | TypeKind::BuiltinStruct(_)
                | TypeKind::Array(_)
                | TypeKind::AccelerationStructure(_)
                | TypeKind::Reference(_)
                | TypeKind::Pointer(_) => {
                    diagnostic_builder(AddressSpaceError::HandleOrTexture);
                },
            }
        },
        // TODO: validate Immediate https://github.com/wgsl-analyzer/wgsl-analyzer/issues/1419
        // TODO: validate RayPayload
        // TODO: validate IncomingRayPayload
        AddressSpace::Immediate | AddressSpace::RayPayload | AddressSpace::IncomingRayPayload => {},
        AddressSpace::TaskPayload => {
            if !matches!(scope, Scope::Module) {
                diagnostic_builder(AddressSpaceError::Scope(Scope::Module));
            }
            // TODO: https://github.com/wgsl-analyzer/wgsl-analyzer/issues/1360
            // if is_mesh_shader {
            //     if !matches!(access_mode, AccessMode::Read) {
            //         diagnostic_builder(AddressSpaceError::AccessMode(smallvec![AccessMode::Read]));
            //     }
            // }
            if !unref.is_error(db) && unref.size_of(address_space, db) < Some(4) {
                diagnostic_builder(AddressSpaceError::TaskPayloadCompatible);
            }
        },
    }
}
