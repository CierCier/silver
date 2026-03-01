use super::*;
use crate::types::TypeId;

#[test]
fn heap_alloc_and_free_are_safe() {
    let mut rt = Runtime::new();

    let h = rt.heap.alloc(object::Object::String("hello".to_string()));
    assert_eq!(rt.heap.allocated_count(), 1);

    let obj = rt.heap.free(h).unwrap();
    assert!(matches!(obj, object::Object::String(_)));
    assert_eq!(rt.heap.allocated_count(), 0);

    // Old handle should be invalid after free (generational)
    assert!(rt.heap.get(h).is_err());
}

#[test]
fn gc_collects_unreachable_objects() {
    let mut rt = Runtime::new();

    let s1 = rt.heap.alloc(object::Object::String("root".to_string()));
    let s2 = rt.heap.alloc(object::Object::String("garbage".to_string()));

    let roots = vec![value::RtValue::Obj {
        ty: TypeId::STRING,
        handle: s1,
    }];

    let freed = rt.heap.collect_garbage(&roots);
    assert_eq!(freed, 1);
    assert_eq!(rt.heap.allocated_count(), 1);

    // s1 still valid
    assert!(rt.heap.get(s1).is_ok());
    // s2 should be invalid after GC sweep
    assert!(rt.heap.get(s2).is_err());
}

#[test]
fn gc_traces_through_composite_objects() {
    let mut rt = Runtime::new();

    let inner = rt.heap.alloc(object::Object::String("inner".to_string()));
    let outer = rt.heap.alloc(object::Object::Vec {
        ty: rt.types.vec_of(TypeId::STRING).unwrap(),
        items: vec![value::RtValue::Obj {
            ty: TypeId::STRING,
            handle: inner,
        }],
    });

    let roots = vec![value::RtValue::Obj {
        ty: rt.types.vec_of(TypeId::STRING).unwrap(),
        handle: outer,
    }];

    let freed = rt.heap.collect_garbage(&roots);
    assert_eq!(freed, 0);
    assert!(rt.heap.get(inner).is_ok());
    assert!(rt.heap.get(outer).is_ok());
}

#[test]
fn builtin_string_len_method_works() {
    let mut rt = Runtime::new();
    let s = rt.alloc_string("hello");
    let out = rt.call_method(s, "len", &[]).unwrap();
    assert_eq!(out.type_id(), TypeId::I64);
    match out {
        value::RtValue::Int { value, .. } => assert_eq!(value, 5),
        other => panic!("expected Int, got {other:?}"),
    }
}

#[test]
fn builtin_vec_push_and_len_work() {
    let mut rt = Runtime::new();
    let vec_ty = rt.types.vec_of(TypeId::I64).unwrap();
    let handle = rt.heap.alloc(object::Object::Vec {
        ty: vec_ty,
        items: vec![],
    });
    let v = value::RtValue::Obj { ty: vec_ty, handle };

    rt.call_method(
        v.clone(),
        "push",
        &[value::RtValue::Int {
            ty: TypeId::I64,
            value: 123,
        }],
    )
    .unwrap();

    let len = rt.call_method(v, "len", &[]).unwrap();
    match len {
        value::RtValue::Int { value, .. } => assert_eq!(value, 1),
        other => panic!("expected Int, got {other:?}"),
    }
}

#[test]
fn custom_struct_method_can_be_registered() {
    let mut rt = Runtime::new();
    let point_ty = rt
        .types
        .define_struct("Point", vec![("x", TypeId::I64), ("y", TypeId::I64)])
        .unwrap();

    rt.register_method(
        point_ty,
        "sum",
        Some(0),
        std::sync::Arc::new(move |rt, receiver, _args| {
            let value::RtValue::Obj { handle, .. } = receiver else {
                return Err(error::RuntimeError::InvalidReceiver {
                    expected: "Point".to_string(),
                });
            };
            let obj = rt.heap.get(handle)?;
            let object::Object::Struct { ty, fields } = obj else {
                return Err(error::RuntimeError::InvalidReceiver {
                    expected: "Point".to_string(),
                });
            };
            if *ty != point_ty {
                return Err(error::RuntimeError::InvalidReceiver {
                    expected: "Point".to_string(),
                });
            }
            let (a, b) = (&fields[0], &fields[1]);
            let (value::RtValue::Int { value: av, .. }, value::RtValue::Int { value: bv, .. }) =
                (a, b)
            else {
                return Err(error::RuntimeError::TypeMismatch {
                    expected: "(i64,i64)".to_string(),
                    found: "<non-int>".to_string(),
                });
            };
            Ok(value::RtValue::Int {
                ty: TypeId::I64,
                value: *av + *bv,
            })
        }),
    );

    let handle = rt.heap.alloc(object::Object::Struct {
        ty: point_ty,
        fields: vec![
            value::RtValue::Int {
                ty: TypeId::I64,
                value: 2,
            },
            value::RtValue::Int {
                ty: TypeId::I64,
                value: 3,
            },
        ],
    });

    let recv = value::RtValue::Obj {
        ty: point_ty,
        handle,
    };
    let out = rt.call_method(recv, "sum", &[]).unwrap();
    match out {
        value::RtValue::Int { value, .. } => assert_eq!(value, 5),
        other => panic!("expected Int, got {other:?}"),
    }
}

#[test]
fn builtin_numeric_casts_work() {
    let mut rt = Runtime::new();

    let out = rt
        .cast_value(
            value::RtValue::Int {
                ty: TypeId::I32,
                value: 42,
            },
            TypeId::I64,
        )
        .unwrap();
    assert_eq!(out.type_id(), TypeId::I64);

    let out = rt
        .cast_value(
            value::RtValue::Int {
                ty: TypeId::I32,
                value: 42,
            },
            TypeId::F64,
        )
        .unwrap();
    assert_eq!(out.type_id(), TypeId::F64);

    let out = rt
        .cast_value(
            value::RtValue::Float {
                ty: TypeId::F64,
                value: 3.14,
            },
            TypeId::I32,
        )
        .unwrap();
    assert_eq!(out.type_id(), TypeId::I32);

    let out = rt
        .cast_value(value::RtValue::Bool(true), TypeId::I32)
        .unwrap();
    assert_eq!(out.type_id(), TypeId::I32);

    let out = rt
        .cast_value(
            value::RtValue::Int {
                ty: TypeId::I32,
                value: 0,
            },
            TypeId::BOOL,
        )
        .unwrap();
    assert_eq!(out.type_id(), TypeId::BOOL);
}

#[test]
fn missing_cast_reports_error() {
    let mut rt = Runtime::new();
    let value = rt.alloc_string("hello");
    let err = rt.cast_value(value, TypeId::I32).unwrap_err();
    assert!(matches!(err, error::RuntimeError::CastNotFound { .. }));
}
