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
                context.custom_width_int_type((bytes * 8) as u32).as_basic_type_enum()
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
        self.classify_fields(
            target_data,
            struct_ty.as_basic_type_enum(),
            &mut classes,
        );

        let align = target_data.get_abi_alignment(&struct_ty);
        let (a, b) = self.combine_classes(classes, align as u64);

        let a_ty = self.class_to_llvm_type(context, a);
        let b_ty = self.class_to_llvm_type(context, b);

        context.struct_type(&[a_ty, b_ty], false).as_basic_type_enum()
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
            // Small struct: pass as integer of exact bit width
            let bits = (size * 8) as u32;
            // LLVM requires at least 1 bit
            let bits = if bits == 0 { 1 } else { bits };
            context.custom_width_int_type(bits).as_basic_type_enum()
        } else if size <= 16 {
            // Medium struct: classify into eightbytes
            self.build_abi_struct(context, target_data, struct_ty)
        } else {
            // Large struct: pass by reference
            context.ptr_type(inkwell::AddressSpace::default()).as_basic_type_enum()
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

/// Factory function to get the appropriate ABI handler for a target triple.
///
/// Currently supports:
/// - x86_64 (amd64): System V AMD64 ABI
///
/// Future support:
/// - aarch64 (arm64): AAPCS64 ABI
/// - x86_64-pc-windows-msvc: Windows x64 ABI
pub fn get_abi_handler(target_triple: &str) -> Box<dyn AbiHandler> {
    if target_triple.contains("x86_64") || target_triple.contains("amd64") {
        Box::new(Amd64Abi::new())
    } else if target_triple.contains("aarch64")
        || target_triple.contains("arm64")
        || target_triple.contains("armv8")
    {
        // ARM64 support would go here
        // For now, fall back to AMD64 as a reasonable default
        // TODO: Implement Arm64Abi
        Box::new(Amd64Abi::new())
    } else {
        // Default to AMD64 for unknown targets
        Box::new(Amd64Abi::new())
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use inkwell::context::Context;
    use inkwell::targets::{CodeModel, InitializationConfig, RelocMode, Target, TargetMachine, TargetTriple};
    use inkwell::OptimizationLevel;

    fn setup_target_machine() -> TargetMachine {
        Target::initialize_x86(&InitializationConfig::default());
        let triple = TargetTriple::create("x86_64-unknown-linux-gnu");
        Target::from_triple(&triple)
            .unwrap()
            .create_target_machine(
                &triple,
                "",
                "",
                OptimizationLevel::None,
                RelocMode::PIC,
                CodeModel::Default,
            )
            .unwrap()
    }

    #[test]
    fn test_abi_handler_name() {
        let handler = Amd64Abi::new();
        assert_eq!(handler.name(), "amd64");
    }

    #[test]
    fn test_classify_fields_simple_struct() {
        let machine = setup_target_machine();
        let tdata = machine.get_target_data();
        let context = Context::create();

        let handler = Amd64Abi::new();
        let struct_ty = context.struct_type(
            &[
                context.f32_type().as_basic_type_enum(),
                context.f32_type().as_basic_type_enum(),
                context.f32_type().as_basic_type_enum(),
            ],
            false,
        );

        let mut classes = Vec::new();
        handler.classify_fields(&tdata, struct_ty.as_basic_type_enum(), &mut classes);

        assert_eq!(
            classes,
            vec![
                AbiClass::Float(4),
                AbiClass::Float(4),
                AbiClass::Float(4),
            ]
        );
    }

    #[test]
    fn test_classify_fields_nested_struct() {
        let machine = setup_target_machine();
        let tdata = machine.get_target_data();
        let context = Context::create();

        let handler = Amd64Abi::new();
        let inner = context.struct_type(
            &[
                context.i32_type().as_basic_type_enum(),
                context.i32_type().as_basic_type_enum(),
            ],
            false,
        );
        let outer = context.struct_type(
            &[
                inner.as_basic_type_enum(),
                context.i64_type().as_basic_type_enum(),
            ],
            false,
        );

        let mut classes = Vec::new();
        handler.classify_fields(&tdata, outer.as_basic_type_enum(), &mut classes);

        assert_eq!(
            classes,
            vec![
                AbiClass::Integer(4),
                AbiClass::Integer(4),
                AbiClass::Integer(8),
            ]
        );
    }

    #[test]
    fn test_combine_classes_all_float() {
        let handler = Amd64Abi::new();
        let (a, b) = handler.combine_classes(
            vec![AbiClass::Float(4), AbiClass::Float(4), AbiClass::Float(4)],
            4,
        );

        // First eightbyte: 4+4 = 8 bytes of float
        // Second eightbyte: 4 bytes of float, padded to 4
        assert_eq!(a, AbiClass::Float(8));
        assert_eq!(b, AbiClass::Float(4));
    }

    #[test]
    fn test_combine_classes_mixed_promotes_to_integer() {
        let handler = Amd64Abi::new();
        let (a, b) = handler.combine_classes(
            vec![
                AbiClass::Float(4),
                AbiClass::Float(4),
                AbiClass::Integer(4),
            ],
            4,
        );

        // First eightbyte: 4+4 = 8 bytes (all float)
        // Second eightbyte: 4 bytes integer
        assert_eq!(a, AbiClass::Float(8));
        assert_eq!(b, AbiClass::Integer(4));
    }

    #[test]
    fn test_combine_classes_mixed_in_first_eightbyte() {
        let handler = Amd64Abi::new();
        let (a, b) = handler.combine_classes(
            vec![
                AbiClass::Float(4),
                AbiClass::Integer(4),
                AbiClass::Integer(4),
                AbiClass::Integer(4),
            ],
            4,
        );

        // First eightbyte: 4+4 = 8 bytes (mixed, promotes to integer)
        // Second eightbyte: 4+4 = 8 bytes (all integer)
        assert_eq!(a, AbiClass::Integer(8));
        assert_eq!(b, AbiClass::Integer(8));
    }

    #[test]
    fn test_combine_classes_with_alignment() {
        let handler = Amd64Abi::new();
        let (a, b) = handler.combine_classes(
            vec![AbiClass::Integer(1), AbiClass::Integer(4), AbiClass::Integer(8)],
            8,
        );

        // First eightbyte: 1+4 = 5 bytes, padded to 8
        // Second eightbyte: 8 bytes
        assert_eq!(a, AbiClass::Integer(8));
        assert_eq!(b, AbiClass::Integer(8));
    }

    #[test]
    fn test_class_to_llvm_type() {
        let context = Context::create();
        let handler = Amd64Abi::new();

        assert_eq!(
            handler.class_to_llvm_type(&context, AbiClass::Integer(1)),
            context.i8_type().as_basic_type_enum()
        );
        assert_eq!(
            handler.class_to_llvm_type(&context, AbiClass::Integer(4)),
            context.i32_type().as_basic_type_enum()
        );
        assert_eq!(
            handler.class_to_llvm_type(&context, AbiClass::Integer(8)),
            context.i64_type().as_basic_type_enum()
        );
        assert_eq!(
            handler.class_to_llvm_type(&context, AbiClass::Float(4)),
            context.f32_type().as_basic_type_enum()
        );
        assert_eq!(
            handler.class_to_llvm_type(&context, AbiClass::Float(8)),
            context.f64_type().as_basic_type_enum()
        );
    }

    #[test]
    fn test_classify_argument_small_struct() {
        let machine = setup_target_machine();
        let tdata = machine.get_target_data();
        let context = Context::create();

        let handler = Amd64Abi::new();
        let struct_ty = context.struct_type(
            &[context.i32_type().as_basic_type_enum()],
            false,
        );

        let result = handler.classify_argument(&context, &tdata, struct_ty);
        assert_eq!(result, context.i32_type().as_basic_type_enum());
    }

    #[test]
    fn test_classify_argument_vector3() {
        let machine = setup_target_machine();
        let tdata = machine.get_target_data();
        let context = Context::create();

        let handler = Amd64Abi::new();
        // Vector3: { float x, float y, float z } = 12 bytes
        let struct_ty = context.struct_type(
            &[
                context.f32_type().as_basic_type_enum(),
                context.f32_type().as_basic_type_enum(),
                context.f32_type().as_basic_type_enum(),
            ],
            false,
        );

        let result = handler.classify_argument(&context, &tdata, struct_ty);

        // Should be a struct of two eightbytes, not i96
        assert!(result.is_struct_type(), "Vector3 should be classified as a struct, not i96");
        let result_struct = result.into_struct_type();
        assert_eq!(result_struct.count_fields(), 2, "Vector3 should have 2 eightbyte fields");
    }

    #[test]
    fn test_classify_argument_large_struct() {
        let machine = setup_target_machine();
        let tdata = machine.get_target_data();
        let context = Context::create();

        let handler = Amd64Abi::new();
        // Large struct: 3 x i64 = 24 bytes
        let struct_ty = context.struct_type(
            &[
                context.i64_type().as_basic_type_enum(),
                context.i64_type().as_basic_type_enum(),
                context.i64_type().as_basic_type_enum(),
            ],
            false,
        );

        let result = handler.classify_argument(&context, &tdata, struct_ty);
        assert!(result.is_pointer_type(), "Large struct should be passed as pointer");
    }

    #[test]
    fn test_needs_byval() {
        let handler = Amd64Abi::new();
        assert!(!handler.needs_byval(8));
        assert!(!handler.needs_byval(16));
        assert!(handler.needs_byval(17));
        assert!(handler.needs_byval(24));
    }

    #[test]
    fn test_needs_sret() {
        let handler = Amd64Abi::new();
        assert!(!handler.needs_sret(8));
        assert!(!handler.needs_sret(16));
        assert!(handler.needs_sret(17));
        assert!(handler.needs_sret(24));
    }

    #[test]
    fn test_get_abi_handler_x86_64() {
        let handler = get_abi_handler("x86_64-unknown-linux-gnu");
        assert_eq!(handler.name(), "amd64");
    }

    #[test]
    fn test_get_abi_handler_aarch64() {
        // Currently falls back to AMD64
        let handler = get_abi_handler("aarch64-unknown-linux-gnu");
        assert_eq!(handler.name(), "amd64");
    }
}
