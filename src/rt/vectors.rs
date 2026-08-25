//! Vector semantics: `VECREF`/`VECSET`/`NEWVEC`.

use crate::gc::Heap;
use crate::rt::wrong_type;
use crate::value::Value;
use crate::value::object::Vector;
use crate::vm::error::{VmError, VmErrorKind};

/// A sanity ceiling on `make-vector`, well past any real program: without one, a typo'd
/// length is an out-of-memory abort inside `Vec` rather than a Scheme error.
const MAX_VECTOR_LEN: i64 = 1 << 32;

fn index_of(heap: &Heap, op: &'static str, idx: Value, len: usize) -> Result<usize, VmError> {
    let Some(i) = idx.as_fixnum() else {
        return Err(wrong_type(heap, op, "an exact integer index", idx));
    };
    match usize::try_from(i) {
        Ok(i) if i < len => Ok(i),
        _ => VmErrorKind::IndexOutOfBounds { index: i, len }.err(),
    }
}

/// `VECREF`: `(vector-ref vec idx)`.
pub fn vec_ref(heap: &Heap, vec: Value, idx: Value) -> Result<Value, VmError> {
    let Some(v) = heap.get::<Vector>(vec) else {
        return Err(wrong_type(heap, "vector-ref", "a vector", vec));
    };
    let i = index_of(heap, "vector-ref", idx, v.elems.len())?;
    Ok(heap
        .get::<Vector>(vec)
        .and_then(|v| v.elems.get(i).copied())
        .unwrap_or(Value::UNDEFINED))
}

/// `VECSET`: `(vector-set! vec idx value)`.
pub fn vec_set(heap: &mut Heap, vec: Value, idx: Value, value: Value) -> Result<(), VmError> {
    let len = match heap.get::<Vector>(vec) {
        Some(v) => v.elems.len(),
        None => return Err(wrong_type(heap, "vector-set!", "a vector", vec)),
    };
    let i = index_of(heap, "vector-set!", idx, len)?;
    if let Some(v) = heap.get_mut::<Vector>(vec)
        && let Some(slot) = v.elems.get_mut(i)
    {
        *slot = value;
    }
    heap.wb(vec, value);
    Ok(())
}

/// `NEWVEC`: `(make-vector len fill)`.
pub fn make_vector(heap: &mut Heap, len: Value, fill: Value) -> Result<Value, VmError> {
    let Some(n) = len.as_fixnum() else {
        return Err(wrong_type(
            heap,
            "make-vector",
            "an exact integer length",
            len,
        ));
    };
    if !(0..MAX_VECTOR_LEN).contains(&n) {
        return VmErrorKind::IndexOutOfBounds {
            index: n,
            len: MAX_VECTOR_LEN as usize,
        }
        .err();
    }
    Ok(heap.vector(vec![fill; n as usize]))
}

#[cfg(test)]
mod tests {
    use super::*;

    fn fix(n: i64) -> Value {
        Value::fixnum(n).unwrap()
    }

    #[test]
    fn make_ref_set_round_trip() {
        let mut heap = Heap::new();
        let v = make_vector(&mut heap, fix(3), Value::FALSE).unwrap();
        assert_eq!(vec_ref(&heap, v, fix(2)).unwrap(), Value::FALSE);
        vec_set(&mut heap, v, fix(2), Value::TRUE).unwrap();
        assert_eq!(vec_ref(&heap, v, fix(2)).unwrap(), Value::TRUE);
        assert_eq!(vec_ref(&heap, v, fix(0)).unwrap(), Value::FALSE);
    }

    #[test]
    fn bounds_and_types_are_checked() {
        let mut heap = Heap::new();
        let v = make_vector(&mut heap, fix(2), Value::NIL).unwrap();

        assert_eq!(
            vec_ref(&heap, v, fix(2)).unwrap_err().kind,
            VmErrorKind::IndexOutOfBounds { index: 2, len: 2 }
        );
        assert_eq!(
            vec_ref(&heap, v, fix(-1)).unwrap_err().kind,
            VmErrorKind::IndexOutOfBounds { index: -1, len: 2 }
        );
        assert!(vec_ref(&heap, v, Value::flonum(1.0)).is_err());
        assert!(vec_ref(&heap, Value::TRUE, fix(0)).is_err());
        assert!(vec_set(&mut heap, v, fix(9), Value::TRUE).is_err());
        assert!(make_vector(&mut heap, fix(-1), Value::NIL).is_err());
        assert!(make_vector(&mut heap, fix(1 << 40), Value::NIL).is_err());
    }
}
