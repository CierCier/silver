use super::*;

#[test]
fn registry_has_builtins() {
    let reg = TypeRegistry::new();
    assert_eq!(reg.name_of(TypeId::UNIT).unwrap(), "unit");
    assert_eq!(reg.name_of(TypeId::BOOL).unwrap(), "bool");
    assert_eq!(reg.name_of(TypeId::I8).unwrap(), "i8");
    assert_eq!(reg.name_of(TypeId::I16).unwrap(), "i16");
    assert_eq!(reg.name_of(TypeId::I32).unwrap(), "i32");
    assert_eq!(reg.name_of(TypeId::I64).unwrap(), "i64");
    assert_eq!(reg.name_of(TypeId::I128).unwrap(), "i128");
    assert_eq!(reg.name_of(TypeId::U8).unwrap(), "u8");
    assert_eq!(reg.name_of(TypeId::U16).unwrap(), "u16");
    assert_eq!(reg.name_of(TypeId::U32).unwrap(), "u32");
    assert_eq!(reg.name_of(TypeId::U64).unwrap(), "u64");
    assert_eq!(reg.name_of(TypeId::U128).unwrap(), "u128");
    assert_eq!(reg.name_of(TypeId::F32).unwrap(), "f32");
    assert_eq!(reg.name_of(TypeId::F64).unwrap(), "f64");
    assert_eq!(reg.name_of(TypeId::STRING).unwrap(), "string");
}

#[test]
fn vec_and_optional_are_cached() {
    let mut reg = TypeRegistry::new();
    let v1 = reg.vec_of(TypeId::I64).unwrap();
    let v2 = reg.vec_of(TypeId::I64).unwrap();
    assert_eq!(v1, v2);

    let o1 = reg.optional_of(TypeId::STRING).unwrap();
    let o2 = reg.optional_of(TypeId::STRING).unwrap();
    assert_eq!(o1, o2);
}

#[test]
fn struct_definition_and_value_validation() {
    let mut reg = TypeRegistry::new();
    let person_ty = reg
        .define_struct(
            "Person",
            vec![("name", TypeId::STRING), ("age", TypeId::I64)],
        )
        .unwrap();

    let ok = Value::new_struct(
        &reg,
        person_ty,
        vec![Value::String("Ada".into()), Value::i64(42)],
    )
    .unwrap();

    assert_eq!(ok.type_id(), person_ty);

    let err = Value::new_struct(&reg, person_ty, vec![Value::i64(1), Value::i64(2)]).unwrap_err();
    match err {
        ValueError::TypeMismatch { .. } => {}
        other => panic!("expected TypeMismatch, got {other:?}"),
    }
}

#[test]
fn value_display_is_stable() {
    let mut reg = TypeRegistry::new();
    let vec_val =
        Value::new_vec(&mut reg, TypeId::I64, vec![Value::i64(1), Value::i64(2)]).unwrap();
    let printed = reg.display_value(&vec_val).to_string();
    assert!(printed.starts_with("vec<i64>["));
}

#[test]
fn numeric_casts_work() {
    let reg = TypeRegistry::new();

    // i64 -> i8 truncation/wrapping like Rust `as`
    let v = Value::i64(260);
    let cast = reg.cast_value(&v, TypeId::I8).unwrap();
    assert_eq!(cast.type_id(), TypeId::I8);
    match cast {
        Value::Int { value, .. } => assert_eq!(value, 4),
        other => panic!("expected Int, got {other:?}"),
    }

    // i64 -> u8
    let v = Value::i64(-1);
    let cast = reg.cast_value(&v, TypeId::U8).unwrap();
    match cast {
        Value::UInt { value, .. } => assert_eq!(value, 255),
        other => panic!("expected UInt, got {other:?}"),
    }

    // i64 -> f32 rounds to f32
    let v = Value::i64(16777217); // 2^24 + 1 (not exactly representable in f32)
    let cast = reg.cast_value(&v, TypeId::F32).unwrap();
    match cast {
        Value::Float { ty, value } => {
            assert_eq!(ty, TypeId::F32);
            assert_eq!((value as f32) as f64, value);
        }
        other => panic!("expected Float, got {other:?}"),
    }
}

#[test]
fn enum_definition_and_value_validation() {
    let mut reg = TypeRegistry::new();
    let result_ty = reg
        .define_enum(
            "ResultI64",
            vec![("Ok", vec![TypeId::I64]), ("Err", vec![TypeId::STRING])],
        )
        .unwrap();

    // Auto discriminants should be 0..n
    let ok_idx = reg.enum_variant_index(result_ty, "Ok").unwrap().unwrap();
    assert_eq!(
        reg.enum_variant(result_ty, ok_idx)
            .unwrap()
            .unwrap()
            .discriminant,
        0
    );
    let err_idx = reg.enum_variant_index(result_ty, "Err").unwrap().unwrap();
    assert_eq!(
        reg.enum_variant(result_ty, err_idx)
            .unwrap()
            .unwrap()
            .discriminant,
        1
    );

    let ok = Value::new_enum_by_name(&reg, result_ty, "Ok", vec![Value::i64(7)]).unwrap();
    assert_eq!(ok.type_id(), result_ty);

    let err =
        Value::new_enum_by_index(&reg, result_ty, 1, vec![Value::String("nope".into())]).unwrap();
    assert_eq!(err.type_id(), result_ty);

    // Wrong payload type
    let bad = Value::new_enum_by_name(&reg, result_ty, "Ok", vec![Value::String("x".into())])
        .unwrap_err();
    match bad {
        ValueError::TypeMismatch { .. } => {}
        other => panic!("expected TypeMismatch, got {other:?}"),
    }

    // Unknown variant
    let bad = Value::new_enum_by_name(&reg, result_ty, "Missing", vec![]).unwrap_err();
    match bad {
        ValueError::UnknownEnumVariantName { .. } => {}
        other => panic!("expected UnknownEnumVariantName, got {other:?}"),
    }
}

#[test]
fn enum_discriminants_can_be_explicit() {
    let mut reg = TypeRegistry::new();
    let e = reg
        .define_enum_with_discriminants(
            "Tokens",
            vec![
                ("Plus", 43, vec![]),
                ("Int", 1, vec![TypeId::I64]),
                ("Ident", 2, vec![TypeId::STRING]),
            ],
        )
        .unwrap();

    let v = Value::new_enum_by_discriminant(&reg, e, 1, vec![Value::i64(9)]).unwrap();
    assert_eq!(v.type_id(), e);

    let bad = reg.define_enum_with_discriminants("Bad", vec![("A", 1, vec![]), ("B", 1, vec![])]);
    assert!(matches!(
        bad,
        Err(TypeError::DuplicateVariantDiscriminant { .. })
    ));
}

#[test]
fn enum_discriminants_auto_fill_mixed() {
    let mut reg = TypeRegistry::new();
    let e = reg
        .define_enum_autofill_discriminants(
            "Mixed",
            vec![
                ("A", Some(5), vec![]),
                ("B", None, vec![]),
                ("C", Some(9), vec![]),
                ("D", None, vec![]),
            ],
        )
        .unwrap();

    let a = reg.enum_variant_index(e, "A").unwrap().unwrap();
    let b = reg.enum_variant_index(e, "B").unwrap().unwrap();
    let c = reg.enum_variant_index(e, "C").unwrap().unwrap();
    let d = reg.enum_variant_index(e, "D").unwrap().unwrap();

    assert_eq!(reg.enum_variant(e, a).unwrap().unwrap().discriminant, 5);
    assert_eq!(reg.enum_variant(e, b).unwrap().unwrap().discriminant, 6);
    assert_eq!(reg.enum_variant(e, c).unwrap().unwrap().discriminant, 9);
    assert_eq!(reg.enum_variant(e, d).unwrap().unwrap().discriminant, 10);
}

#[test]
fn enum_discriminant_autofill_overflow_is_error() {
    let mut reg = TypeRegistry::new();
    let bad = reg.define_enum_autofill_discriminants(
        "Overflow",
        vec![("A", Some(i64::MAX), vec![]), ("B", None, vec![])],
    );
    assert!(matches!(bad, Err(TypeError::DiscriminantOverflow { .. })));
}
