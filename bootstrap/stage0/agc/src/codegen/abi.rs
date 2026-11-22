//! C ABI struct classification for LLVM code generation.
//!
//! A struct crossing an `extern "C"` boundary by value is classified per
//! target into a [`StructPass`]: the LLVM type that carries it in registers,
//! or one of the two by-reference forms. Scalars never come through here.
//!
//! References:
//! - System V AMD64 psABI (https://gitlab.com/x86-psABIs/x86-64-ABI)
//! - Microsoft x64 conventions (https://learn.microsoft.com/en-us/cpp/build/x64-software-conventions)
//! - AAPCS64 (https://github.com/ARM-software/abi-aa/blob/main/aapcs64/aapcs64.rst)
//! - WebAssembly Basic C ABI (https://github.com/WebAssembly/tool-conventions/blob/main/BasicCABI.md)
//!
//! Each handler places a struct in the same registers and stack slots as
//! clang 22. The LLVM carrier type can still differ from clang's, as with
//! `i64` where clang uses `ptr` for an AArch64 struct holding one pointer.
use std::num::NonZeroU32;

use inkwell::context::Context;
use inkwell::targets::TargetData;
use inkwell::types::{BasicType, BasicTypeEnum, FloatType, StructType};
use inkwell::AddressSpace;

/// Which side of a call a C-ABI value is on.
#[derive(Clone, Copy, Debug, PartialEq)]
pub enum AbiSlot {
    Argument,
    Result,
}

/// How one by-value C struct crosses a call.
#[derive(Clone, Copy, Debug, PartialEq)]
pub enum StructPass<'ctx> {
    /// As a value of `ty`, reinterpreted from the struct's bytes. `ty` equal to
    /// the struct type means no reinterpretation. `stack_align` is the
    /// `alignstack` attribute for a value that spills to the stack.
    Direct {
        ty: BasicTypeEnum<'ctx>,
        stack_align: Option<u64>,
    },
    /// As a copy in the caller's outgoing argument area (`byval`). Arguments only.
    ByVal,
    /// Through a pointer to a caller-owned copy. For a result this is the
    /// hidden `sret` slot.
    Indirect,
}

impl<'ctx> StructPass<'ctx> {
    fn direct(ty: impl BasicType<'ctx>) -> Self {
        Self::Direct {
            ty: ty.as_basic_type_enum(),
            stack_align: None,
        }
    }

    /// The LLVM type of the argument or result that carries the struct.
    /// By-reference forms are pointers; an `Indirect` result has no LLVM
    /// result type, and callers handle it through the `sret` slot first.
    pub fn llvm_type(self, context: &'ctx Context) -> BasicTypeEnum<'ctx> {
        match self {
            Self::Direct { ty, .. } => ty,
            Self::ByVal | Self::Indirect => context.ptr_type(AddressSpace::default()).as_basic_type_enum(),
        }
    }
}

/// Target-specific classification of by-value C structs.
pub trait AbiHandler {
    /// How `struct_ty` passes as an argument.
    fn argument<'ctx>(&self, context: &'ctx Context, target_data: &TargetData, struct_ty: StructType<'ctx>)
        -> StructPass<'ctx>;

    /// How `struct_ty` returns. Never `ByVal`.
    fn result<'ctx>(&self, context: &'ctx Context, target_data: &TargetData, struct_ty: StructType<'ctx>)
        -> StructPass<'ctx>;
}

/// An integer of `bytes` bytes (at least one bit, as LLVM requires).
fn int_of_bytes(context: &Context, bytes: u64) -> BasicTypeEnum<'_> {
    let bits = NonZeroU32::new((bytes * 8).max(1) as u32).expect("nonzero width");
    context.custom_width_int_type(bits).expect("valid integer width").as_basic_type_enum()
}

/// Classification of a field for SysV eightbyte packing: INTEGER or SSE,
/// with its byte size.
#[derive(Copy, Clone, Debug, PartialEq)]
enum AbiClass {
    Integer(u64),
    Float(u64),
}

/// System V AMD64 ABI:
///
/// 1. Up to 8 bytes pass as an integer of the exact width, except two `f32`s,
///    which pass as `<2 x float>` in one SSE register.
/// 2. 9-16 bytes split into two eightbytes, each INTEGER unless all of its
///    fields are floats.
/// 3. Larger structs pass `byval` and return through `sret`.
pub struct Amd64Abi;

impl Amd64Abi {
    /// Flattens nested structs and arrays into per-field classes.
    fn classify_fields(target_data: &TargetData, ty: BasicTypeEnum, classes: &mut Vec<AbiClass>) {
        match ty {
            BasicTypeEnum::StructType(t) => {
                for field in t.get_field_types_iter() {
                    Self::classify_fields(target_data, field, classes);
                }
            }
            BasicTypeEnum::ArrayType(t) => {
                let field = t.get_element_type();
                for _ in 0..t.len() {
                    Self::classify_fields(target_data, field, classes);
                }
            }
            BasicTypeEnum::FloatType(t) => classes.push(AbiClass::Float(target_data.get_abi_size(&t))),
            BasicTypeEnum::IntType(t) => classes.push(AbiClass::Integer(target_data.get_abi_size(&t))),
            BasicTypeEnum::PointerType(t) => classes.push(AbiClass::Integer(target_data.get_abi_size(&t))),
            BasicTypeEnum::VectorType(_) | BasicTypeEnum::ScalableVectorType(_) => {
                panic!("vector types are not yet supported in ABI classification");
            }
        }
    }

    /// Packs field classes into two eightbytes; an eightbyte holding any
    /// INTEGER field is INTEGER.
    fn combine_classes(classes: Vec<AbiClass>, align: u64) -> (AbiClass, AbiClass) {
        let (mut a_size, mut a_is_float) = (0, true);
        let (mut b_size, mut b_is_float) = (0, true);
        for class in classes {
            match class {
                AbiClass::Integer(v) if a_size + v <= 8 => {
                    a_size += v;
                    a_is_float = false;
                }
                AbiClass::Float(v) if a_size + v <= 8 => a_size += v,
                AbiClass::Integer(v) => {
                    b_size += v;
                    b_is_float = false;
                }
                AbiClass::Float(v) => b_size += v,
            }
        }
        let class = |size: u64, is_float| {
            let size = size.max(align);
            if is_float { AbiClass::Float(size) } else { AbiClass::Integer(size) }
        };
        (class(a_size, a_is_float), class(b_size, b_is_float))
    }

    fn class_type(context: &Context, class: AbiClass) -> BasicTypeEnum<'_> {
        match class {
            AbiClass::Integer(bytes) => int_of_bytes(context, bytes),
            AbiClass::Float(4) => context.f32_type().as_basic_type_enum(),
            AbiClass::Float(_) => context.f64_type().as_basic_type_enum(),
        }
    }
}

impl AbiHandler for Amd64Abi {
    fn argument<'ctx>(&self, context: &'ctx Context, target_data: &TargetData, struct_ty: StructType<'ctx>)
        -> StructPass<'ctx> {
        let size = target_data.get_abi_size(&struct_ty);
        if size <= 8 {
            let fields = struct_ty.get_field_types();
            if size == 8 && fields.len() == 2 && fields.iter().all(|f| f.is_float_type()) {
                return StructPass::direct(context.f32_type().vec_type(2));
            }
            StructPass::direct(int_of_bytes(context, size))
        } else if size <= 16 {
            let mut classes = Vec::new();
            Self::classify_fields(target_data, struct_ty.as_basic_type_enum(), &mut classes);
            let (a, b) = Self::combine_classes(classes, target_data.get_abi_alignment(&struct_ty) as u64);
            StructPass::direct(context.struct_type(&[Self::class_type(context, a), Self::class_type(context, b)], false))
        } else {
            StructPass::ByVal
        }
    }

    fn result<'ctx>(&self, context: &'ctx Context, target_data: &TargetData, struct_ty: StructType<'ctx>)
        -> StructPass<'ctx> {
        match self.argument(context, target_data, struct_ty) {
            StructPass::ByVal => StructPass::Indirect,
            pass => pass,
        }
    }
}

/// Microsoft x64 ABI: structs of exactly 1, 2, 4 or 8 bytes pass and return
/// as one integer register. Everything else passes as a caller copy and
/// returns through `sret`. There is no eightbyte classification and no
/// float-register case for aggregates.
pub struct Win64Abi;

impl Win64Abi {
    fn in_register<'ctx>(context: &'ctx Context, target_data: &TargetData, struct_ty: StructType<'ctx>)
        -> Option<StructPass<'ctx>> {
        let size = target_data.get_abi_size(&struct_ty);
        matches!(size, 1 | 2 | 4 | 8).then(|| StructPass::direct(int_of_bytes(context, size)))
    }
}

impl AbiHandler for Win64Abi {
    fn argument<'ctx>(&self, context: &'ctx Context, target_data: &TargetData, struct_ty: StructType<'ctx>)
        -> StructPass<'ctx> {
        Self::in_register(context, target_data, struct_ty).unwrap_or(StructPass::ByVal)
    }

    fn result<'ctx>(&self, context: &'ctx Context, target_data: &TargetData, struct_ty: StructType<'ctx>)
        -> StructPass<'ctx> {
        Self::in_register(context, target_data, struct_ty).unwrap_or(StructPass::Indirect)
    }
}

/// AAPCS64 (aarch64 Linux):
///
/// 1. A homogeneous floating-point aggregate (HFA: one to four members of one
///    float type, through nested structs and arrays) passes as `[N x T]` in
///    consecutive FP registers and returns as itself in `s0`-`s3`/`d0`-`d3`.
/// 2. Other structs of up to 16 bytes pass as `i64`, `[2 x i64]`, or `i128`
///    when 16-byte aligned, and return as an exact-width integer when they fit
///    8 bytes.
/// 3. Larger structs pass as a pointer to a caller copy (not `byval`) and
///    return through `sret`, which AArch64 places in `x8`.
pub struct Arm64Abi;

impl Arm64Abi {
    /// The member float type and count of an HFA, or `None` when `struct_ty`
    /// is not one.
    fn homogeneous_float<'ctx>(target_data: &TargetData, struct_ty: StructType<'ctx>)
        -> Option<(FloatType<'ctx>, u32)> {
        fn visit<'ctx>(ty: BasicTypeEnum<'ctx>, base: &mut Option<FloatType<'ctx>>, count: &mut u32) -> bool {
            match ty {
                BasicTypeEnum::FloatType(float) => {
                    *count += 1;
                    *base.get_or_insert(float) == float
                }
                BasicTypeEnum::StructType(inner) => inner.get_field_types_iter().all(|f| visit(f, base, count)),
                BasicTypeEnum::ArrayType(array) => {
                    // Walk the element once; a huge array must not cost a walk
                    // per element before the four-member limit rejects it.
                    let mut members = 0;
                    if array.len() > 0 && !visit(array.get_element_type(), base, &mut members) {
                        return false;
                    }
                    *count = count.saturating_add(members.saturating_mul(array.len()));
                    true
                }
                _ => false,
            }
        }
        let (mut base, mut count) = (None, 0);
        if !visit(struct_ty.as_basic_type_enum(), &mut base, &mut count) {
            return None;
        }
        let base = base?;
        // Explicit padding between members would disqualify the aggregate.
        let packed = target_data.get_abi_size(&struct_ty) == target_data.get_abi_size(&base) * u64::from(count);
        ((1..=4).contains(&count) && packed).then_some((base, count))
    }
}

impl AbiHandler for Arm64Abi {
    fn argument<'ctx>(&self, context: &'ctx Context, target_data: &TargetData, struct_ty: StructType<'ctx>)
        -> StructPass<'ctx> {
        let size = target_data.get_abi_size(&struct_ty);
        if size == 0 {
            // An empty struct occupies no register or stack slot.
            return StructPass::direct(struct_ty);
        }
        if let Some((base, count)) = Self::homogeneous_float(target_data, struct_ty) {
            return StructPass::Direct {
                ty: base.array_type(count).as_basic_type_enum(),
                stack_align: Some(8),
            };
        }
        if size > 16 {
            return StructPass::Indirect;
        }
        let unit: u64 = if target_data.get_abi_alignment(&struct_ty) >= 16 { 16 } else { 8 };
        let units = size.div_ceil(unit);
        let unit_ty = int_of_bytes(context, unit);
        if units == 1 {
            StructPass::direct(unit_ty)
        } else {
            StructPass::direct(unit_ty.array_type(units as u32))
        }
    }

    fn result<'ctx>(&self, context: &'ctx Context, target_data: &TargetData, struct_ty: StructType<'ctx>)
        -> StructPass<'ctx> {
        let size = target_data.get_abi_size(&struct_ty);
        if size == 0 || Self::homogeneous_float(target_data, struct_ty).is_some() {
            return StructPass::direct(struct_ty);
        }
        if size > 16 {
            return StructPass::Indirect;
        }
        if size <= 8 {
            return StructPass::direct(int_of_bytes(context, size));
        }
        if target_data.get_abi_alignment(&struct_ty) >= 16 {
            StructPass::direct(context.i128_type())
        } else {
            StructPass::direct(context.i64_type().array_type(2))
        }
    }
}

/// WebAssembly Basic C ABI: a struct with exactly one member, recursively
/// through single-member structs and one-element arrays, passes and returns
/// as that member's type. Every other struct passes as a caller copy and
/// returns through `sret`.
pub struct WasmAbi;

impl WasmAbi {
    /// The scalar a single-member chain bottoms out in, or `None` when the
    /// chain reaches an aggregate with more than one member.
    fn single_scalar(struct_ty: StructType) -> Option<BasicTypeEnum> {
        let mut current = struct_ty.as_basic_type_enum();
        loop {
            match current {
                BasicTypeEnum::ArrayType(array) if array.len() == 1 => current = array.get_element_type(),
                BasicTypeEnum::StructType(inner) if inner.count_fields() == 1 => {
                    current = inner.get_field_types()[0];
                }
                BasicTypeEnum::ArrayType(_) | BasicTypeEnum::StructType(_) => return None,
                scalar => return Some(scalar),
            }
        }
    }
}

impl AbiHandler for WasmAbi {
    fn argument<'ctx>(&self, _context: &'ctx Context, _target_data: &TargetData, struct_ty: StructType<'ctx>)
        -> StructPass<'ctx> {
        Self::single_scalar(struct_ty).map_or(StructPass::ByVal, StructPass::direct)
    }

    fn result<'ctx>(&self, _context: &'ctx Context, _target_data: &TargetData, struct_ty: StructType<'ctx>)
        -> StructPass<'ctx> {
        Self::single_scalar(struct_ty).map_or(StructPass::Indirect, StructPass::direct)
    }
}

/// The struct ABI handler for `target_triple`, or `None` when struct passing
/// is not implemented for its architecture. Scalar-only C calls still work
/// there; a by-value struct at a C boundary is a compile error rather than a
/// guess.
pub fn get_abi_handler(target_triple: &str) -> Option<Box<dyn AbiHandler>> {
    let triple = target_triple.to_ascii_lowercase();
    let arch = triple.split('-').next().unwrap_or_default();
    if target_is_wasm(Some(&triple)) {
        Some(Box::new(WasmAbi))
    } else if matches!(arch, "x86_64" | "amd64") {
        if target_is_windows(Some(&triple)) {
            Some(Box::new(Win64Abi))
        } else {
            Some(Box::new(Amd64Abi))
        }
    } else if matches!(arch, "aarch64" | "arm64") && !target_is_windows(Some(&triple)) && !target_is_darwin(&triple) {
        // Darwin and Windows arm64 deviate from AAPCS64 (Darwin variadics and
        // stack packing, Windows HFA variadics); neither is a Silver target.
        Some(Box::new(Arm64Abi))
    } else {
        None
    }
}

/// Whether `triple` names an Apple OS, by vendor or by OS component, as LLVM
/// treats `aarch64-unknown-darwin` like `arm64-apple-darwin`.
fn target_is_darwin(triple: &str) -> bool {
    triple.split('-').skip(1).any(|part| {
        part == "apple" || ["darwin", "macos", "ios", "tvos", "watchos", "xros", "visionos"]
            .iter().any(|os| part.starts_with(os))
    })
}

/// Returns true when the target triple denotes a WebAssembly target.
///
/// Shared by codegen/link/driver passes that must branch on the architecture
/// component of the triple rather than the host OS.
pub fn target_is_wasm(target_triple: Option<&str>) -> bool {
    match target_triple {
        Some(triple) => {
            let t = triple.to_ascii_lowercase();
            t.starts_with("wasm32") || t.starts_with("wasm64") || t.contains("wasm")
        }
        None => false,
    }
}

/// Returns true when the target triple denotes a Windows target.
///
/// Shared by codegen/link/driver passes that must branch on the OS component
/// of the triple rather than the host OS.
pub fn target_is_windows(target_triple: Option<&str>) -> bool {
    match target_triple {
        Some(triple) => {
            let t = triple.to_ascii_lowercase();
            t.contains("windows") || t.contains("win32") || t.contains("mingw")
        }
        None => cfg!(target_os = "windows"),
    }
}
