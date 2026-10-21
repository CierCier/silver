/// ABI classification and struct passing for LLVM code generation.
///
/// This module implements the System V AMD64 ABI struct classification algorithm,
/// which determines how structs are passed as function arguments and return values.
///
/// The module is designed to be extensible for other ABIs (ARM64, Windows x64, etc.)
/// through the `AbiHandler` trait.
///
/// References:
/// - System V Application Binary Interface, AMD64 Architecture Processor Supplement
///   (https://gitlab.com/x86-psABIs/x86-64-ABI)
/// - Inko compiler ABI implementation (https://github.com/inko-lang/inko)
use std::num::NonZeroU32;

use inkwell::targets::TargetData;
use inkwell::types::{BasicType, BasicTypeEnum, StructType};

/// Classification of a field for ABI purposes.
///
/// The System V AMD64 ABI classifies each field as either INTEGER or FLOAT,
/// along with its byte size. These classes are then packed into "eightbytes"
/// (8-byte slots) for passing in registers.
#[derive(Copy, Clone, Debug, PartialEq)]
pub enum AbiClass {
    /// Integer class: integers, pointers, and mixed types.
    /// The value is the byte size.
    Integer(u64),
    /// Floating-point class: float and double.
    /// The value is the byte size (4 or 8).
    Float(u64),
}

impl AbiClass {
    /// Returns true if this class is a floating-point class.
    pub fn is_float(self) -> bool {
        matches!(self, AbiClass::Float(_))
    }

    /// Returns the byte size of this class.
    pub fn size(self) -> u64 {
        match self {
            AbiClass::Integer(s) | AbiClass::Float(s) => s,
        }
    }
}

/// Trait for ABI-specific struct classification.
///
/// Implement this trait to add support for new ABIs. The trait provides
/// methods for classifying struct arguments and return values according
/// to the ABI's rules.
pub trait AbiHandler {
    /// Returns the name of this ABI (e.g., "amd64", "arm64").
    fn name(&self) -> &str;

    /// Classifies a struct type for use as a function argument.
    ///
    /// Returns the LLVM type that should be used in the function signature.
    /// For structs <= 8 bytes, this is typically an integer of the appropriate width.
    /// For structs 9-16 bytes, this is a struct of up to two eightbytes.
    /// For structs > 16 bytes, this is a pointer (with `byval` attribute).
    fn classify_argument<'ctx>(
        &self,
        context: &'ctx inkwell::context::Context,
        target_data: &TargetData,
        struct_ty: StructType<'ctx>,
    ) -> BasicTypeEnum<'ctx>;

    /// Classifies a struct type for use as a function return value.
    ///
    /// Similar to `classify_argument`, but may have different rules for
    /// certain ABIs (e.g., ARM64 returns small structs as integers of
    /// exact bit width rather than rounded up).
    fn classify_return<'ctx>(
        &self,
        context: &'ctx inkwell::context::Context,
        target_data: &TargetData,
        struct_ty: StructType<'ctx>,
    ) -> BasicTypeEnum<'ctx>;

    /// Returns true if this ABI requires `byval` attribute for the given struct size.
    fn needs_byval(&self, size: u64) -> bool;

    /// Returns true if this ABI requires `sret` attribute for struct returns of the given size.
    fn needs_sret(&self, size: u64) -> bool;

    /// Type-aware by-reference decision. The size alone cannot express wasm's
    /// rule (a `{i64}` aggregate passes by value while `{i32,i32}` — the same
    /// 8 bytes — passes by pointer), so callers that know the struct type use
    /// this; the default falls back to the size-only predicate.
    fn struct_needs_byval<'ctx>(
        &self,
        _context: &'ctx inkwell::context::Context,
        target_data: &TargetData,
        struct_ty: StructType<'ctx>,
    ) -> bool {
        self.needs_byval(target_data.get_store_size(&struct_ty))
    }

    /// Type-aware `sret` decision; see `struct_needs_byval`.
    fn struct_needs_sret<'ctx>(
        &self,
        _context: &'ctx inkwell::context::Context,
        target_data: &TargetData,
        struct_ty: StructType<'ctx>,
    ) -> bool {
        self.needs_sret(target_data.get_store_size(&struct_ty))
    }

    /// Returns the alignment to use for `byval`/`sret` attributes.
    fn byval_alignment(&self, struct_ty: StructType, target_data: &TargetData) -> u64;
}

/// System V AMD64 ABI handler.
///
/// Implements the struct classification rules from the System V AMD64 ABI:
///
/// 1. If the size is <= 8 bytes, pass as an integer of the exact bit width.
/// 2. If the size is 9-16 bytes:
///    a. Flatten the struct into individual fields.
///    b. Classify each field as INTEGER or FLOAT.
///    c. Pack fields into two eightbytes (max 8 bytes each).
///    d. If an eightbyte contains mixed INTEGER and FLOAT, promote to INTEGER.
/// 3. If the size is > 16 bytes, pass by reference (pointer with `byval`).
///
/// For returns, the rules are the same, except that > 16 byte structs use
/// the `sret` (struct return) attribute on an implicit first parameter.
pub struct Amd64Abi;

impl Default for Amd64Abi {
    fn default() -> Self {
        Self
    }
}

impl Amd64Abi {
    /// Creates a new AMD64 ABI handler.
    pub fn new() -> Self {
        Self
    }

    /// Recursively classifies fields of a type into AbiClass entries.
    ///
    /// This flattens nested structs and arrays into individual field classifications.
    fn classify_fields(
        &self,
        target_data: &TargetData,
        ty: BasicTypeEnum,
        classes: &mut Vec<AbiClass>,
    ) {
        match ty {
            BasicTypeEnum::StructType(t) => {
                for field in t.get_field_types_iter() {
                    self.classify_fields(target_data, field, classes);
                }
            }
            BasicTypeEnum::ArrayType(t) => {
                let field = t.get_element_type();
                for _ in 0..t.len() {
                    self.classify_fields(target_data, field, classes);
                }
            }
            BasicTypeEnum::FloatType(t) => {
                classes.push(AbiClass::Float(target_data.get_abi_size(&t)));
            }
            BasicTypeEnum::IntType(t) => {
                classes.push(AbiClass::Integer(target_data.get_abi_size(&t)));
            }
            BasicTypeEnum::PointerType(t) => {
                classes.push(AbiClass::Integer(target_data.get_abi_size(&t)));
            }
            BasicTypeEnum::VectorType(_) | BasicTypeEnum::ScalableVectorType(_) => {
                // Vector types are not yet supported in Silver.
                // If they are added, they would be classified as INTEGER.
                panic!("vector types are not yet supported in ABI classification");
            }
        }
    }

    /// Combines classified fields into two eightbytes.
    ///
    /// This implements the "merging" step of the AMD64 ABI classification.
    /// Fields are packed into the first eightbyte until it reaches 8 bytes,
    /// then the remainder goes into the second eightbyte.
    ///
    /// If an eightbyte contains a mix of INTEGER and FLOAT classes, the
    /// entire eightbyte is promoted to INTEGER.
    fn combine_classes(&self, classes: Vec<AbiClass>, align: u64) -> (AbiClass, AbiClass) {
        let mut a_size: u64 = 0;
        let mut a_is_float = true;
        let mut b_size: u64 = 0;
        let mut b_is_float = true;

        for cls in classes {
            match cls {
                AbiClass::Integer(v) if a_size + v <= 8 => {
                    a_size += v;
                    a_is_float = false;
                }
                AbiClass::Float(v) if a_size + v <= 8 => {
                    a_size += v;
                }
                AbiClass::Integer(v) => {
                    b_size += v;
                    b_is_float = false;
                }
                AbiClass::Float(v) => {
                    b_size += v;
                }
            }
        }

        // Apply alignment padding
        use std::cmp::max;
        a_size = max(a_size, align);
        b_size = max(b_size, align);

        (
            if a_is_float {
                AbiClass::Float(a_size)
            } else {
                AbiClass::Integer(a_size)
            },
            if b_is_float {
                AbiClass::Float(b_size)
            } else {
                AbiClass::Integer(b_size)
            },
        )
    }

    /// Converts an AbiClass to its corresponding LLVM type.
    fn class_to_llvm_type<'ctx>(
        &self,
        context: &'ctx inkwell::context::Context,
        cls: AbiClass,
    ) -> BasicTypeEnum<'ctx> {
        match cls {
            AbiClass::Integer(bytes) => {
                let bits = NonZeroU32::new((bytes * 8) as u32).unwrap();
                context
                    .custom_width_int_type(bits)
                    .unwrap()
                    .as_basic_type_enum()
            }
            AbiClass::Float(4) => context.f32_type().as_basic_type_enum(),
            AbiClass::Float(_) => context.f64_type().as_basic_type_enum(),
        }
    }

    /// Builds the ABI struct type for a 9-16 byte struct.
    ///
    /// This creates a struct of up to two fields based on the eightbyte classification.
    fn build_abi_struct<'ctx>(
        &self,
        context: &'ctx inkwell::context::Context,
        target_data: &TargetData,
        struct_ty: StructType<'ctx>,
    ) -> BasicTypeEnum<'ctx> {
        let mut classes = Vec::new();
        self.classify_fields(target_data, struct_ty.as_basic_type_enum(), &mut classes);

        let align = target_data.get_abi_alignment(&struct_ty);
        let (a, b) = self.combine_classes(classes, align as u64);

        let a_ty = self.class_to_llvm_type(context, a);
        let b_ty = self.class_to_llvm_type(context, b);

        context
            .struct_type(&[a_ty, b_ty], false)
            .as_basic_type_enum()
    }
}

impl AbiHandler for Amd64Abi {
    fn name(&self) -> &str {
        "amd64"
    }

    fn classify_argument<'ctx>(
        &self,
        context: &'ctx inkwell::context::Context,
        target_data: &TargetData,
        struct_ty: StructType<'ctx>,
    ) -> BasicTypeEnum<'ctx> {
        let size = target_data.get_abi_size(&struct_ty);

        if size <= 8 {
            // SysV SSE class: a struct of exactly two floats (8 bytes) is
            // passed as a single <2 x float> in one XMM register.
            let fields = struct_ty.get_field_types();
            if size == 8 && fields.len() == 2 && fields.iter().all(|f| f.is_float_type()) {
                return context.f32_type().vec_type(2).as_basic_type_enum();
            }
            // Small struct: pass as integer of exact bit width
            let bits = (size * 8) as u32;
            // LLVM requires at least 1 bit
            let bits = if bits == 0 { 1 } else { bits };
            context
                .custom_width_int_type(NonZeroU32::new(bits).unwrap())
                .unwrap()
                .as_basic_type_enum()
        } else if size <= 16 {
            // Medium struct: classify into eightbytes
            self.build_abi_struct(context, target_data, struct_ty)
        } else {
            // Large struct: pass by reference
            context
                .ptr_type(inkwell::AddressSpace::default())
                .as_basic_type_enum()
        }
    }

    fn classify_return<'ctx>(
        &self,
        context: &'ctx inkwell::context::Context,
        target_data: &TargetData,
        struct_ty: StructType<'ctx>,
    ) -> BasicTypeEnum<'ctx> {
        // For AMD64, return classification is the same as argument classification
        self.classify_argument(context, target_data, struct_ty)
    }

    fn needs_byval(&self, size: u64) -> bool {
        size > 16
    }

    fn needs_sret(&self, size: u64) -> bool {
        size > 16
    }

    fn byval_alignment(&self, struct_ty: StructType, target_data: &TargetData) -> u64 {
        target_data.get_abi_alignment(&struct_ty) as u64
    }
}

/// Windows x64 ABI handler.
///
/// Implements the Microsoft x64 calling convention struct rules
/// (https://learn.microsoft.com/en-us/cpp/build/x64-software-conventions):
///
/// 1. Aggregates of size exactly 1, 2, 4, or 8 bytes pass by value in a single
///    integer register (rcx/rdx/r8/r9) — no eightbyte classification, and no
///    single-float XMM coercion (clang/MSVC pass one-float aggregates in the
///    integer class; XMM is reserved for scalar float/double arguments).
///    Other sizes (3, 5, 6, 7, and everything > 8) pass by reference: the
///    caller makes a temporary copy and passes a pointer (modeled as a pointer
///    parameter with the `byval` attribute).
/// 2. There is no 9-16-byte two-eightbyte register case.
///
/// Returns: 1/2/4/8-byte aggregates in `rax`, everything else via a hidden
/// `sret` pointer (caller-allocated, returned in `rax`).
///
/// The 32-byte shadow space at call sites, register-assignment ordering
/// (floats occupy XMM slots by ordinal position), and `.pdata`/`.xdata` unwind
/// info are handled by LLVM's backend for windows triples; this handler only
/// shapes the LLVM type used in function signatures.
pub struct Win64Abi;

impl Default for Win64Abi {
    fn default() -> Self {
        Self
    }
}

impl Win64Abi {
    /// Creates a new Win64 ABI handler.
    pub fn new() -> Self {
        Self
    }

    /// Win64 passes aggregates by value only at exactly 1, 2, 4, or 8 bytes.
    fn passes_by_value(size: u64) -> bool {
        matches!(size, 1 | 2 | 4 | 8)
    }
}

impl AbiHandler for Win64Abi {
    fn name(&self) -> &str {
        "win64"
    }

    fn classify_argument<'ctx>(
        &self,
        context: &'ctx inkwell::context::Context,
        target_data: &TargetData,
        struct_ty: StructType<'ctx>,
    ) -> BasicTypeEnum<'ctx> {
        let size = target_data.get_abi_size(&struct_ty);

        if Self::passes_by_value(size) {
            // Small aggregate: single integer register
            let bits = (size * 8) as u32;
            context
                .custom_width_int_type(NonZeroU32::new(bits).unwrap())
                .unwrap()
                .as_basic_type_enum()
        } else {
            // Everything else: by reference (caller-made temporary copy + pointer)
            context
                .ptr_type(inkwell::AddressSpace::default())
                .as_basic_type_enum()
        }
    }

    fn classify_return<'ctx>(
        &self,
        context: &'ctx inkwell::context::Context,
        target_data: &TargetData,
        struct_ty: StructType<'ctx>,
    ) -> BasicTypeEnum<'ctx> {
        let size = target_data.get_abi_size(&struct_ty);

        if Self::passes_by_value(size) {
            let bits = (size * 8) as u32;
            context
                .custom_width_int_type(NonZeroU32::new(bits).unwrap())
                .unwrap()
                .as_basic_type_enum()
        } else {
            // Hidden sret pointer
            context
                .ptr_type(inkwell::AddressSpace::default())
                .as_basic_type_enum()
        }
    }

    fn needs_byval(&self, size: u64) -> bool {
        !Self::passes_by_value(size)
    }

    fn needs_sret(&self, size: u64) -> bool {
        !Self::passes_by_value(size)
    }

    fn byval_alignment(&self, struct_ty: StructType, target_data: &TargetData) -> u64 {
        target_data.get_abi_alignment(&struct_ty) as u64
    }
}

/// WebAssembly (wasm32/wasm64) Basic C ABI handler.
///
/// Implements the struct rules from the WebAssembly tool-conventions
/// "Basic C ABI" (https://github.com/WebAssembly/tool-conventions/blob/main/BasicCABI.md):
///
/// 1. Aggregates with exactly one member (recursively: a single field, a
///    single-element array of it, or a nested single-member struct) pass by
///    value with LLVM field extraction: the aggregate signature type is the
///    member's own type ({i32}→i32, {f32}→float, {i64}→i64, {i128}→i128,
///    {f64}→double). Clang 22 verified: `struct {int a[1];}` → i32,
///    `struct {__int128 x;}` → i128, `struct {double x;}` → double.
/// 2. Every other aggregate passes by reference: the caller makes a copy in
///    the frame and passes a pointer (modeled as pointer + `byval`).
/// 3. Returns mirror the argument rules; multi-member returns use a hidden
///    `sret` pointer.
///
/// Scalars (including i128) pass directly; variadics use plain LLVM varargs.
pub struct WasmAbi;

impl Default for WasmAbi {
    fn default() -> Self {
        Self
    }
}

impl WasmAbi {
    /// Creates a new Wasm ABI handler.
    pub fn new() -> Self {
        Self
    }

    /// True when the struct has exactly one direct member (fields for struct
    /// types, one element for `[1 x T]` arrays). The wasm ABI recurses into
    /// that single member when classifying.
    fn single_member(struct_ty: StructType) -> bool {
        struct_ty.count_fields() == 1
    }

    /// Flattens a single-member chain down to the first non-single-member
    /// level, returning the LLVM type that would be extracted for by-value
    /// passing. Returns `None` when the chain bottoms out in a multi-member
    /// aggregate (use byval instead).
    fn extracted_field_type(struct_ty: StructType) -> Option<BasicTypeEnum> {
        let mut current = struct_ty.as_basic_type_enum();
        loop {
            match current {
                BasicTypeEnum::ArrayType(arr) => {
                    if arr.len() != 1 {
                        return None;
                    }
                    current = arr.get_element_type();
                }
                BasicTypeEnum::StructType(inner) => {
                    if !Self::single_member(inner) {
                        return None;
                    }
                    // Exactly one field: descend into it.
                    current = inner.get_field_types()[0];
                }
                other => return Some(other),
            }
        }
    }
}

impl AbiHandler for WasmAbi {
    fn name(&self) -> &str {
        "wasm"
    }

    fn classify_argument<'ctx>(
        &self,
        context: &'ctx inkwell::context::Context,
        _target_data: &TargetData,
        struct_ty: StructType<'ctx>,
    ) -> BasicTypeEnum<'ctx> {
        match Self::extracted_field_type(struct_ty) {
            Some(field_ty) => field_ty,
            None => context
                .ptr_type(inkwell::AddressSpace::default())
                .as_basic_type_enum(),
        }
    }

    fn classify_return<'ctx>(
        &self,
        context: &'ctx inkwell::context::Context,
        target_data: &TargetData,
        struct_ty: StructType<'ctx>,
    ) -> BasicTypeEnum<'ctx> {
        self.classify_argument(context, target_data, struct_ty)
    }

    fn needs_byval(&self, _size: u64) -> bool {
        // Size-only answers are ambiguous on wasm ({i64} and {i32,i32} are
        // both 8 bytes but classify differently). The type-aware decision is
        // made via `struct_needs_byval`/`struct_needs_sret`; this size-based
        // predicate is the conservative fallback used when only a size is
        // known (matches the multi-member default).
        true
    }

    fn needs_sret(&self, _size: u64) -> bool {
        true
    }

    fn struct_needs_byval<'ctx>(
        &self,
        _context: &'ctx inkwell::context::Context,
        _target_data: &TargetData,
        struct_ty: StructType<'ctx>,
    ) -> bool {
        // Single-member aggregates (recursively) pass by value; everything
        // else passes by reference.
        Self::extracted_field_type(struct_ty).is_none()
    }

    fn struct_needs_sret<'ctx>(
        &self,
        _context: &'ctx inkwell::context::Context,
        _target_data: &TargetData,
        struct_ty: StructType<'ctx>,
    ) -> bool {
        // A single-member aggregate returns its member directly, no sret.
        Self::extracted_field_type(struct_ty).is_none()
    }

    fn byval_alignment(&self, struct_ty: StructType, target_data: &TargetData) -> u64 {
        target_data.get_abi_alignment(&struct_ty) as u64
    }
}

/// Type-aware wasm classification used by the signature/attribute emitters:
/// returns the extracted by-value field type for single-member aggregates
/// (`Some`) or `None` when the aggregate must pass via byval/sret. Non-struct
/// types never need coercion.
pub fn wasm_extracted_field<'ctx>(
    _target_data: &TargetData,
    lowered: BasicTypeEnum<'ctx>,
) -> Option<BasicTypeEnum<'ctx>> {
    match lowered {
        BasicTypeEnum::StructType(struct_ty) => WasmAbi::extracted_field_type(struct_ty),
        _ => None,
    }
}

/// Factory function to get the appropriate ABI handler for a target triple.
///
/// Dispatch considers both the architecture and the OS component: the same
/// x86_64 architecture uses System V (Linux/macOS/BSD) or Win64 (Windows)
/// struct-passing rules, and they are NOT compatible.
///
/// Currently supports:
/// - x86_64 linux/mac/bsd: System V AMD64 ABI
/// - x86_64 windows: Win64 (Microsoft x64) ABI
/// - wasm32/wasm64: WebAssembly Basic C ABI
///
/// Future support:
/// - aarch64 (arm64): AAPCS64 ABI
pub fn get_abi_handler(target_triple: &str) -> Box<dyn AbiHandler> {
    let triple = target_triple.to_ascii_lowercase();
    let is_windows = target_is_windows(Some(&triple));
    if target_is_wasm(Some(&triple)) {
        Box::new(WasmAbi::new())
    } else if triple.contains("x86_64") || triple.contains("amd64") {
        if is_windows {
            Box::new(Win64Abi::new())
        } else {
            Box::new(Amd64Abi::new())
        }
    } else if triple.contains("aarch64")
        || triple.contains("arm64")
        || triple.contains("armv8")
    {
        // ARM64 support would go here
        // For now, fall back to AMD64 as a reasonable default
        // TODO(#17): Implement Arm64Abi — AAPCS64 struct classification
        // differs from AMD64; this fallback silently mis-compiles aarch64
        // targets with struct arguments/returns.
        Box::new(Amd64Abi::new())
    } else {
        // Default to AMD64 for unknown targets
        Box::new(Amd64Abi::new())
    }
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
