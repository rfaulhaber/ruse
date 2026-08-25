//! The runtime: opcode and primitive semantics as standalone functions.
//!
//! This split is habit 2 of `docs/project_plan.org`: the dispatch loop's arms decode
//! operands and call in here, so every opcode's behaviour is unit-testable without
//! booting a VM, the loop stays small enough to read, and a future second consumer (a
//! baseline JIT lowering the same bytecode) calls these functions instead of growing a
//! second implementation of the language.

pub mod arith;
pub mod equal;
pub mod pairs;
pub mod prims;
pub mod vectors;
pub mod write;

use crate::gc::Heap;
use crate::value::Value;
use crate::value::layout::HeapTag;
use crate::vm::error::{VmError, VmErrorKind};

/// The name of `v`'s type, for error messages.
pub fn type_name(heap: &Heap, v: Value) -> &'static str {
    if v.is_flonum() || v.is_fixnum() {
        return "number";
    }
    if v.is_char() {
        return "character";
    }
    if v.is_boolean() {
        return "boolean";
    }
    if v.is_null() {
        return "empty list";
    }
    if v.is_eof() {
        return "eof object";
    }
    if v.is_unspecified() {
        return "unspecified value";
    }
    if v.is_undefined() {
        return "undefined value";
    }
    match heap.tag_of(v) {
        Some(HeapTag::Pair) => "pair",
        Some(HeapTag::Str) => "string",
        Some(HeapTag::Symbol) => "symbol",
        Some(HeapTag::Vector) => "vector",
        Some(HeapTag::Bytevector) => "bytevector",
        Some(HeapTag::Closure | HeapTag::NativeProc) => "procedure",
        Some(HeapTag::UpvalueCell) => "upvalue cell",
        Some(HeapTag::Bignum) => "number",
        Some(HeapTag::Record) => "record",
        Some(HeapTag::RecordType) => "record type",
        None => "unknown value",
    }
}

/// The standard wrong-type refusal, named after the operation that noticed.
pub(crate) fn wrong_type(
    heap: &Heap,
    op: &'static str,
    expected: &'static str,
    got: Value,
) -> VmError {
    VmError::new(VmErrorKind::WrongType {
        op,
        expected,
        got: type_name(heap, got),
    })
}
