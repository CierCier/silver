//! Centralized catalog of compiler diagnostic messages, warnings, notes, and error strings.
//!
//! All user-facing compiler strings are defined here in one location, making them
//! easy to audit, edit, customize, and translate (i18n/l10n).

use std::fmt::Display;

// =========================================================================
// General & Suggestions
// =========================================================================

pub fn did_you_mean(suggestion: &str) -> String {
    format!(", did you mean '{suggestion}'?")
}

// =========================================================================
// Semantic & Analyzer Messages
// =========================================================================

pub fn unknown_type(name: &str, suggestion: &str) -> String {
    format!("unknown type '{name}'{suggestion}")
}

pub fn unknown_trait(name: &str, suggestion: &str) -> String {
    format!("unknown trait '{name}'{suggestion}")
}

pub fn not_a_trait(name: &str) -> String {
    format!("'{name}' is not a trait")
}

pub fn duplicate_symbol(name: &str) -> String {
    format!("duplicate symbol '{name}'")
}

pub fn inherent_drop_not_trait_impl() -> &'static str {
    "inherent method 'drop' is not a Drop trait impl; consider 'impl Drop<T> for T' instead"
}

pub fn duplicate_variable(name: &str) -> String {
    format!("duplicate variable '{name}'")
}
// =========================================================================
// Type Checker Messages
// =========================================================================

pub fn unknown_identifier(name: &str, suggestion: &str) -> String {
    format!("unknown identifier '{name}'{suggestion}")
}

pub fn unknown_function(name: &str, suggestion: &str) -> String {
    format!("unknown function '{name}'{suggestion}")
}

pub fn unknown_field(field: &str, ty: &impl Display, suggestion: &str) -> String {
    format!("unknown field '{field}' on type {ty}{suggestion}")
}

pub fn unknown_enum(name: &str, suggestion: &str) -> String {
    format!("unknown enum '{name}'{suggestion}")
}

pub fn unknown_enum_type(name: &str, suggestion: &str) -> String {
    format!("unknown enum type '{name}'{suggestion}")
}

pub fn unknown_builtin_macro(name: &str, suggestion: &str) -> String {
    format!("unknown builtin macro '@{name}'{suggestion}")
}

pub fn type_mismatch(expected: &impl Display, found: &impl Display) -> String {
    format!("type mismatch: expected {expected}, found {found}")
}

pub fn return_type_mismatch(expected: &impl Display, found: &impl Display) -> String {
    format!("return type mismatch: expected {expected}, found {found}")
}

pub fn return_type_mismatch_unit(expected: &impl Display) -> String {
    format!("return type mismatch: expected {expected}, found unit")
}

pub fn void_func_cannot_return_value() -> &'static str {
    "void function cannot return a value"
}

pub fn return_not_allowed_in_defer() -> &'static str {
    "return statement is not allowed inside a defer block"
}

pub fn break_not_allowed_in_defer() -> &'static str {
    "break statement is not allowed inside a defer block"
}

pub fn no_matching_overload(
    method: &str,
    arg_desc: &str,
    candidates: &[String],
    suggestion: &str,
) -> String {
    let mut msg = format!("no matching overload for '{method}', given {arg_desc}");
    if !candidates.is_empty() {
        msg.push_str(", expected one of: ");
        msg.push_str(&candidates.join(", "));
    } else if !suggestion.is_empty() {
        msg.push_str(suggestion);
    }
    msg
}

pub fn ambiguous_overload(name: &str, candidates: &[String]) -> String {
    let mut msg = format!("ambiguous overload for '{name}'");
    if candidates.len() > 1 {
        msg.push_str(", candidates: ");
        msg.push_str(&candidates.join(", "));
    }
    msg
}

pub fn not_callable(name: &str) -> String {
    format!("'{name}' is not callable")
}

pub fn type_not_callable(name: &str) -> String {
    format!("type '{name}' is not callable: no function with that name")
}

pub fn expr_not_callable(ty: &impl Display) -> String {
    format!("expression is not callable (type {ty})")
}

pub fn function_arg_count_mismatch(name: &str, expected: usize, got: usize) -> String {
    format!("function '{name}' expected {expected} arguments, got {got}")
}

pub fn expected_args_got(expected: usize, got: usize) -> String {
    format!("expected {expected} arguments, got {got}")
}

pub fn variant_arg_count_mismatch(variant: &str, expected: usize, got: usize) -> String {
    format!("variant '{variant}' expects {expected} arguments, got {got}")
}

pub fn enum_variant_field_count_mismatch(
    variant: &str,
    enum_name: &str,
    expected: usize,
    got: usize,
) -> String {
    format!("enum variant '{variant}' of '{enum_name}' expects {expected} fields, got {got}")
}

pub fn payload_must_be_moved(ty: &impl Display) -> String {
    format!("payload of type '{ty}' must be moved into the enum (it owns a resource)")
}

pub fn enum_members_type_scoped() -> &'static str {
    "enum members must be accessed through the enum type name"
}

pub fn local_bindings_need_type_annotation() -> &'static str {
    "local bindings must include a type annotation"
}

pub fn struct_literals_not_supported() -> &'static str {
    "struct literal expressions are not supported here; use a variable initializer ('Type id = { .field = value }') instead"
}

pub fn single_binding_let_only() -> &'static str {
    "let declarations must bind a single identifier; destructuring patterns are not supported"
}

// =========================================================================
// Move & Escape Checker Messages
// =========================================================================

pub fn use_of_moved_value(name: &str) -> String {
    format!("use of moved value '{name}'")
}

pub fn cannot_assign_to_moved_value(name: &str) -> String {
    format!("cannot assign to moved value '{name}'")
}

pub fn note_value_explicitly_moved() -> &'static str {
    "value explicitly moved here"
}

pub fn note_value_moved_by_return() -> &'static str {
    "value moved by return"
}

pub fn note_value_consumed_by_method() -> &'static str {
    "value consumed by by-value method call"
}

pub fn note_value_moved_into_param() -> &'static str {
    "value moved into by-value parameter"
}

pub fn note_value_moved_into_launch() -> &'static str {
    "value moved into thread launch"
}

pub fn note_task_handle_consumed() -> &'static str {
    "task handle consumed by wait"
}

pub fn cannot_borrow_as_mutable_while_shared(name: &str) -> String {
    format!("cannot borrow '{name}' as mutable because it is already borrowed as shared")
}

pub fn cannot_borrow_as_mutable_more_than_once(name: &str) -> String {
    format!("cannot borrow '{name}' as mutable more than once at a time")
}

pub fn cannot_borrow_as_shared_while_mutable(name: &str) -> String {
    format!("cannot borrow '{name}' as shared because it is already borrowed as mutable")
}

pub fn cannot_assign_to_borrowed(name: &str) -> String {
    format!("cannot assign to '{name}' because it is borrowed")
}

pub fn cannot_move_out_of_borrowed(name: &str) -> String {
    format!("cannot move out of '{name}' because it is borrowed")
}

pub fn cannot_use_mutably_borrowed(name: &str) -> String {
    format!("cannot use '{name}' because it was mutably borrowed")
}

pub fn note_previous_borrow_here(kind: &str) -> String {
    format!("previous {kind} borrow occurs here")
}

pub fn returned_reference_escapes() -> &'static str {
    "returned reference does not outlive the function (it borrows a local value; return a reference to a global or a reference parameter instead)"
}

pub fn reference_stored_into_global(name: &str) -> String {
    format!(
        "reference to a local value stored into global '{name}' — it would dangle after the function returns"
    )
}

pub fn cannot_send_type(ty: &impl Display, reason: &str) -> String {
    format!("cannot send type '{ty}' across thread boundary: {reason}")
}

// =========================================================================
// Linter & Warning Messages
// =========================================================================

pub fn unused_variable(name: &str) -> String {
    format!("unused variable '{name}' (help: prefix with _ to ignore: '_{name}')")
}

pub fn unused_parameter(name: &str) -> String {
    format!("unused parameter '{name}' (help: prefix with _ to ignore: '_{name}')")
}

pub fn unreachable_statement() -> &'static str {
    "unreachable statement"
}

// =========================================================================
// Builtin Macro Messages
// =========================================================================

pub fn macro_requires_min_args(name: &str, min: usize) -> String {
    format!("`@{name}` requires at least {min} argument(s)")
}

pub fn macro_expected_format_args(name: &str, expected: usize, got: usize) -> String {
    format!("`@{name}` expected {expected} format argument(s), got {got}")
}

pub fn invalid_postfix_operator() -> &'static str {
    "invalid postfix operator"
}
pub fn implicit_guard_missing(symbol: &str, left: &impl Display, right: &impl Display) -> String {
    format!(
        "generic call requires '{left} {symbol} {right}', but no overload exists for that operator"
    )
}
pub fn implicit_method_guard_missing(
    name: &str,
    receiver: &impl Display,
    args: &[String],
) -> String {
    format!(
        "generic call requires method '{name}' on '{receiver}' with arguments ({})",
        args.join(", ")
    )
}
pub fn invalid_cast(from: &impl Display, to: &impl Display) -> String {
    format!("invalid cast: {from} -> {to}")
}
pub fn move_operand_identifier() -> &'static str {
    "move operand must be an identifier"
}
pub fn wait_requires_task(ty: &impl Display) -> String {
    format!("'wait' requires a Task, got {ty}")
}
pub fn type_expression_not_callable() -> &'static str {
    "type expression is not callable"
}
pub fn size_exactly_one() -> &'static str {
    "@size expects exactly one argument"
}
pub fn size_expression() -> &'static str {
    "@size requires an expression argument"
}
pub fn align_exactly_one() -> &'static str {
    "@align expects exactly one argument"
}
pub fn align_expression() -> &'static str {
    "@align requires an expression argument"
}
pub fn hash_exactly_one() -> &'static str {
    "@hash expects exactly one argument"
}
pub fn hash_expression() -> &'static str {
    "@hash requires an expression argument"
}
pub fn json_expression() -> &'static str {
    "@json requires an expression value"
}
pub fn json_prefix_type() -> &'static str {
    "@json prefix must have type str"
}
pub fn json_prefix_string() -> &'static str {
    "@json prefix must be a string expression"
}
pub fn from_json_target() -> &'static str {
    "@from_json requires a target type"
}
pub fn from_json_named_target() -> &'static str {
    "@from_json requires a named target type"
}
pub fn from_json_input() -> &'static str {
    "@from_json requires an input expression"
}
pub fn from_json_input_type() -> &'static str {
    "@from_json input must have type str"
}
pub fn duplicate_binding(name: &str) -> String {
    format!("duplicate binding for '{name}'")
}
pub fn cannot_assign_const(name: &str) -> String {
    format!("cannot assign to const variable '{name}'")
}
pub fn cannot_assign_const_field(name: &str) -> String {
    format!("cannot assign to field of const variable '{name}'")
}
pub fn continue_not_allowed_in_defer() -> &'static str {
    "continue statement is not allowed inside a defer block"
}
pub fn target_feature_exactly_one() -> &'static str {
    "#[target_feature] expects exactly one argument"
}
pub fn target_feature_string_literal() -> &'static str {
    "#[target_feature] requires a string literal"
}
pub fn unknown_target_feature(name: &str) -> String {
    format!(
        "unknown target feature '{name}': expected one of sse41, sse42, popcnt, fma, avx, avx2, avx512f"
    )
}
pub fn json_arg_count() -> &'static str {
    "@json expects a value and optional string prefix"
}
pub fn from_json_arg_count() -> &'static str {
    "@from_json expects a target type and input string"
}
pub fn enum_discriminant_overflow(name: &str) -> String {
    format!("enum variant '{name}' overflows automatic discriminant resolution")
}

pub fn format_string_must_be_literal() -> &'static str {
    "format string must be a literal"
}

// =========================================================================
// Attributes, Config & Parser Messages
// =========================================================================

pub fn cfg_requires_at_least_one_argument() -> &'static str {
    "#[cfg] requires at least one argument"
}

pub fn where_clause_requires_predicate() -> &'static str {
    "where clause requires at least one predicate"
}

pub fn inline_only_always() -> &'static str {
    "#[inline] only supports #[inline(always)]"
}

pub fn link_name_expects_one() -> &'static str {
    "#[link_name] expects exactly one argument"
}

pub fn inline_expects_one() -> &'static str {
    "#[inline] expects exactly one argument (always)"
}

pub fn assignment_type_mismatch(expected: &impl Display, found: &impl Display) -> String {
    format!("assignment type mismatch: {expected} = {found}")
}

pub fn memcpy_expects_three() -> &'static str {
    "@memcpy expects exactly 3 arguments (dst, src, len)"
}

pub fn memset_expects_three() -> &'static str {
    "@memset expects exactly 3 arguments (dst, value, len)"
}

pub fn memmove_expects_three() -> &'static str {
    "@memmove expects exactly 3 arguments (dst, src, len)"
}
