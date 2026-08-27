//! Pair semantics: `CONS`/`CAR`/`CDR`/`SETCAR`/`SETCDR` and the list helpers.

use crate::gc::Heap;
use crate::rt::wrong_type;
use crate::value::Value;
use crate::value::object::Pair;
use crate::vm::error::VmError;

/// `CAR`: the first component of a pair.
pub fn car(heap: &Heap, v: Value) -> Result<Value, VmError> {
    match heap.get::<Pair>(v).map(|p| p.car) {
        Some(car) => Ok(car),
        None => Err(wrong_type(heap, "car", "a pair", v)),
    }
}

/// `CDR`: the second component of a pair.
pub fn cdr(heap: &Heap, v: Value) -> Result<Value, VmError> {
    match heap.get::<Pair>(v).map(|p| p.cdr) {
        Some(cdr) => Ok(cdr),
        None => Err(wrong_type(heap, "cdr", "a pair", v)),
    }
}

/// `SETCAR`: `(set-car! pair v)`.
pub fn set_car(heap: &mut Heap, pair: Value, v: Value) -> Result<(), VmError> {
    match heap.get_mut::<Pair>(pair) {
        Some(p) => {
            p.car = v;
            heap.wb(pair, v);
            Ok(())
        }
        None => Err(wrong_type(heap, "set-car!", "a pair", pair)),
    }
}

/// `SETCDR`: `(set-cdr! pair v)`.
pub fn set_cdr(heap: &mut Heap, pair: Value, v: Value) -> Result<(), VmError> {
    match heap.get_mut::<Pair>(pair) {
        Some(p) => {
            p.cdr = v;
            heap.wb(pair, v);
            Ok(())
        }
        None => Err(wrong_type(heap, "set-cdr!", "a pair", pair)),
    }
}

/// A fresh proper list of `items`.
///
/// Consed back to front, so the intermediate list is complete at every step — no
/// half-built cell is ever exposed to a collection.
pub fn list(heap: &mut Heap, items: &[Value]) -> Value {
    let mut acc = Value::NIL;
    for &item in items.iter().rev() {
        acc = heap.cons(item, acc);
    }
    acc
}

/// The elements of the proper list `v`, in order. `op` names the caller in the
/// wrong-type refusal for an improper (or non-) list. Diverges on a cyclic list, like
/// every M3/M4 list walk; cycle handling is an M9 conformance question.
pub fn list_elements(heap: &Heap, op: &'static str, v: Value) -> Result<Vec<Value>, VmError> {
    let mut elems = Vec::new();
    let mut cur = v;
    while !cur.is_null() {
        let Some(p) = heap.get::<Pair>(cur) else {
            return Err(wrong_type(heap, op, "a proper list", v));
        };
        elems.push(p.car);
        cur = p.cdr;
    }
    Ok(elems)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::vm::error::VmErrorKind;

    #[test]
    fn car_and_cdr_read_a_pair_and_refuse_everything_else() {
        let mut heap = Heap::new();
        let p = heap.cons(Value::TRUE, Value::NIL);
        assert_eq!(car(&heap, p).unwrap(), Value::TRUE);
        assert_eq!(cdr(&heap, p).unwrap(), Value::NIL);

        let err = car(&heap, Value::NIL).unwrap_err();
        assert_eq!(
            err.kind,
            VmErrorKind::WrongType {
                op: "car",
                expected: "a pair",
                got: "empty list"
            }
        );
        assert!(cdr(&heap, Value::fixnum(1).unwrap()).is_err());
    }

    #[test]
    fn mutation_writes_through_with_the_barrier() {
        let mut heap = Heap::new();
        let p = heap.cons(Value::NIL, Value::NIL);
        set_car(&mut heap, p, Value::TRUE).unwrap();
        set_cdr(&mut heap, p, Value::FALSE).unwrap();
        assert_eq!(car(&heap, p).unwrap(), Value::TRUE);
        assert_eq!(cdr(&heap, p).unwrap(), Value::FALSE);
        assert!(set_car(&mut heap, Value::TRUE, Value::NIL).is_err());
    }

    #[test]
    fn list_builds_in_order() {
        let mut heap = Heap::new();
        let items = [
            Value::fixnum(1).unwrap(),
            Value::fixnum(2).unwrap(),
            Value::fixnum(3).unwrap(),
        ];
        let mut l = list(&mut heap, &items);
        for want in items {
            assert_eq!(car(&heap, l).unwrap(), want);
            l = cdr(&heap, l).unwrap();
        }
        assert!(l.is_null());
        assert_eq!(list(&mut heap, &[]), Value::NIL);
    }
}
