//! End-to-end tests of the walking skeleton: source text → reader → compiler → verifier
//! → dispatch loop → value. These are the M3 exit criteria in executable form — most
//! prominently that tail recursion runs in *constant* frame depth, proved by running a
//! million-iteration loop under a frame limit of fifty.

// Integration tests are separate crates, so the crate-root `cfg_attr(test, allow(...))` in
// the library does not reach them. Asserting with `unwrap`, and panicking with context in
// the helpers, is the point of a test.
#![allow(clippy::unwrap_used, clippy::panic)]

use std::cell::RefCell;
use std::io::Write;
use std::rc::Rc;

use ruse::value::layout::HeapTag;
use ruse::{RuseError, Value, Vm, VmErrorKind};

/// A capture buffer the test can read after handing the VM its writer.
#[derive(Clone, Default)]
struct Sink(Rc<RefCell<Vec<u8>>>);

impl Write for Sink {
    fn write(&mut self, buf: &[u8]) -> std::io::Result<usize> {
        self.0.borrow_mut().extend_from_slice(buf);
        Ok(buf.len())
    }

    fn flush(&mut self) -> std::io::Result<()> {
        Ok(())
    }
}

impl Sink {
    fn text(&self) -> String {
        String::from_utf8(self.0.borrow().clone()).unwrap()
    }
}

fn vm() -> Vm {
    Vm::with_output(Box::new(Sink::default()))
}

fn eval(vm: &mut Vm, src: &str) -> Value {
    match vm.eval_str(src) {
        Ok(v) => v,
        Err(e) => panic!("eval of {src:?} failed: {e}"),
    }
}

fn eval_err(vm: &mut Vm, src: &str) -> RuseError {
    match vm.eval_str(src) {
        Ok(v) => panic!("eval of {src:?} unexpectedly produced {v:?}"),
        Err(e) => e,
    }
}

fn vm_kind(e: &RuseError) -> &VmErrorKind {
    match e {
        RuseError::Vm(e) => &e.kind,
        other => panic!("expected a runtime error, got {other}"),
    }
}

fn fix(n: i64) -> Value {
    Value::fixnum(n).unwrap()
}

#[test]
fn arithmetic_reaches_a_value() {
    let mut vm = vm();
    assert_eq!(eval(&mut vm, "(+ 1 2 3)"), fix(6));
    assert_eq!(eval(&mut vm, "(- 10 1 2)"), fix(7));
    assert_eq!(eval(&mut vm, "(* 2 3 7)"), fix(42));
    assert_eq!(eval(&mut vm, "(- 5)"), fix(-5));
    assert_eq!(eval(&mut vm, "(+)"), fix(0));
    assert_eq!(eval(&mut vm, "(*)"), fix(1));
    assert_eq!(eval(&mut vm, "(* 2 3.5)").as_flonum(), Some(7.0));
    assert_eq!(eval(&mut vm, "42"), fix(42));
    assert_eq!(eval(&mut vm, "(+ 40 2)"), fix(42));
}

#[test]
fn comparisons_are_values_and_branches() {
    let mut vm = vm();
    assert_eq!(eval(&mut vm, "(< 1 2)"), Value::TRUE);
    assert_eq!(eval(&mut vm, "(> 1 2)"), Value::FALSE);
    assert_eq!(eval(&mut vm, "(>= 2 2)"), Value::TRUE);
    assert_eq!(eval(&mut vm, "(= 3 3)"), Value::TRUE);
    assert_eq!(eval(&mut vm, "(eq? 'a 'a)"), Value::TRUE);
    assert_eq!(eval(&mut vm, "(if (< 1 2) 10 20)"), fix(10));
    assert_eq!(eval(&mut vm, "(if (> 1 2) 10 20)"), fix(20));
    assert!(eval(&mut vm, "(if #f 1)").is_unspecified());
    // Only #f is false.
    assert_eq!(eval(&mut vm, "(if 0 'zero 'no)"), eval(&mut vm, "'zero"));
    assert_eq!(eval(&mut vm, "(if '() 'nil 'no)"), eval(&mut vm, "'nil"));
}

#[test]
fn define_lambda_let_begin_set() {
    let mut vm = vm();
    assert_eq!(
        eval(&mut vm, "(define (double x) (+ x x)) (double 21)"),
        fix(42)
    );
    assert_eq!(
        eval(&mut vm, "(define square (lambda (x) (* x x))) (square 9)"),
        fix(81)
    );
    assert_eq!(eval(&mut vm, "(let ((x 2) (y 3)) (* x y))"), fix(6));
    assert_eq!(eval(&mut vm, "(begin 1 2 3)"), fix(3));
    assert_eq!(eval(&mut vm, "(define x 1) (set! x 41) (+ x 1)"), fix(42));
    // Plain let: inits see the outer scope.
    assert_eq!(
        eval(&mut vm, "(define y 10) (let ((y 2) (z y)) (+ y z))"),
        fix(12)
    );
    // Lambda parameters shadow globals.
    assert_eq!(eval(&mut vm, "(define (shadow x) x) (shadow 7)"), fix(7));
}

#[test]
fn quoting_and_list_structure() {
    let mut vm = vm();
    assert_eq!(eval(&mut vm, "(car '(1 2 3))"), fix(1));
    assert_eq!(eval(&mut vm, "(car (cdr '(1 2 3)))"), fix(2));
    assert!(eval(&mut vm, "(cons 1 2)").is_heap());
    assert_eq!(
        eval(&mut vm, "(equal? '(1 (2 3)) (list 1 (list 2 3)))"),
        Value::TRUE
    );
    assert_eq!(eval(&mut vm, "(equal? '(1 2) '(1 3))"), Value::FALSE);
    assert_eq!(eval(&mut vm, "(eqv? \"a\" \"a\")"), Value::FALSE);
    assert_eq!(eval(&mut vm, "(equal? \"a\" \"a\")"), Value::TRUE);
    assert_eq!(eval(&mut vm, "'()"), Value::NIL);
    assert_eq!(eval(&mut vm, "(null? '())"), Value::TRUE);
    // Nested quotes stay data.
    assert_eq!(eval(&mut vm, "(car ''a)"), eval(&mut vm, "'quote"));
}

#[test]
fn procedures_are_first_class() {
    let mut vm = vm();
    // The spec's own example: primitives are values.
    assert_eq!(eval(&mut vm, "((if #f + *) 3 4)"), fix(12));
    assert_eq!(
        eval(
            &mut vm,
            "(define (call-with f x) (f x)) (call-with car '(9 8))"
        ),
        fix(9)
    );
    assert_eq!(eval(&mut vm, "(procedure? +)"), Value::TRUE);
    assert_eq!(eval(&mut vm, "(procedure? (lambda (x) x))"), Value::TRUE);
}

#[test]
fn redefining_a_primitive_revokes_its_inlining() {
    let mut vm = vm();
    assert_eq!(eval(&mut vm, "(+ 10 1)"), fix(11));
    // After the redefinition, later forms must see the new binding even though earlier
    // ones compiled `+` to ADD.
    assert_eq!(eval(&mut vm, "(define + -) (+ 10 1)"), fix(9));
}

/// IEEE zeros: a one-argument `+`/`*` returns its argument, sign of zero intact, and the
/// native fold agrees with the inlined opcode on the same operands.
#[test]
fn negative_zero_survives_identity_arithmetic() {
    let mut vm = vm();
    assert_eq!(eval(&mut vm, "(eqv? (+ -0.0) -0.0)"), Value::TRUE);
    assert_eq!(eval(&mut vm, "(eqv? (* -0.0) -0.0)"), Value::TRUE);
    assert_eq!(eval(&mut vm, "(eqv? (+ -0.0 -0.0) -0.0)"), Value::TRUE);
    // The native path (unlicensed name) answers exactly like the inlined path.
    assert_eq!(
        eval(
            &mut vm,
            "(define plus +) (eqv? (plus -0.0 -0.0) (+ -0.0 -0.0))"
        ),
        Value::TRUE
    );
    // And the type check still fires with no addition to do.
    let err = eval_err(&mut vm, "(plus \"a\")");
    assert!(matches!(
        vm_kind(&err),
        VmErrorKind::WrongType { op: "+", .. }
    ));
}

#[test]
fn a_lexical_binding_shadows_a_keyword() {
    let mut vm = vm();
    assert_eq!(
        eval(&mut vm, "(let ((if (lambda (a b c) 99))) (if 1 2 3))"),
        fix(99)
    );
}

#[test]
fn fact_20_promotes_to_bignum() {
    let mut vm = vm();
    let v = eval(
        &mut vm,
        "(define (fact n) (if (= n 0) 1 (* n (fact (- n 1))))) (fact 20)",
    );
    assert_eq!(vm.heap().tag_of(v), Some(HeapTag::Bignum));
    assert_eq!(vm.heap().integer_to_i64(v), Some(2_432_902_008_176_640_000));
}

/// The M3 exit criterion: tail recursion in constant frame depth. A frame limit of 50
/// makes frame reuse the only way a million iterations can succeed.
#[test]
fn tail_recursion_runs_in_constant_frame_depth() {
    let mut vm = vm();
    vm.set_frame_limit(50);
    let v = eval(
        &mut vm,
        "(define (loop n) (if (= n 0) 'done (loop (- n 1)))) (loop 1000000)",
    );
    assert_eq!(v, eval(&mut vm, "'done"));

    // The accumulator variant from the spec §6.1, likewise constant-depth.
    let v = eval(
        &mut vm,
        "(define (fact n acc) (if (= n 0) acc (fact (- n 1) (* n acc)))) (fact 10 1)",
    );
    assert_eq!(v, fix(3_628_800));
}

#[test]
fn non_tail_recursion_hits_the_frame_limit_as_a_typed_error() {
    let mut vm = vm();
    vm.set_frame_limit(100);
    eval(
        &mut vm,
        "(define (count n) (if (= n 0) 0 (+ 1 (count (- n 1)))))",
    );
    assert_eq!(eval(&mut vm, "(count 50)"), fix(50));
    let err = eval_err(&mut vm, "(count 200)");
    assert_eq!(vm_kind(&err), &VmErrorKind::StackOverflow { limit: 100 });
}

#[test]
fn output_reaches_the_sink() {
    let sink = Sink::default();
    let mut vm = Vm::with_output(Box::new(sink.clone()));
    eval(&mut vm, "(display \"hi\") (newline) (write \"hi\")");
    eval(&mut vm, "(write '(1 \"two\" #\\c))");
    eval(&mut vm, "(display '(1 \"two\" #\\c))");
    assert_eq!(sink.text(), "hi\n\"hi\"(1 \"two\" #\\c)(1 two c)");
}

#[test]
fn runtime_errors_are_typed_and_carry_spans() {
    let mut vm = vm();

    let err = eval_err(&mut vm, "unbound-thing");
    assert_eq!(
        vm_kind(&err),
        &VmErrorKind::UnboundVariable {
            name: "unbound-thing".to_string()
        }
    );

    let err = eval_err(&mut vm, "(set! never-defined 1)");
    assert!(matches!(
        vm_kind(&err),
        VmErrorKind::UnboundVariable { name } if name == "never-defined"
    ));

    let err = eval_err(&mut vm, "(car 5)");
    assert!(matches!(
        vm_kind(&err),
        VmErrorKind::WrongType { op: "car", .. }
    ));
    if let RuseError::Vm(e) = &err {
        assert!(e.span.is_some(), "the faulting instruction has a span");
    }

    let err = eval_err(&mut vm, "(1 2)");
    assert!(matches!(
        vm_kind(&err),
        VmErrorKind::NotCallable { got: "number" }
    ));

    let err = eval_err(&mut vm, "((lambda (x) x))");
    assert!(matches!(
        vm_kind(&err),
        VmErrorKind::WrongArity { got: 0, .. }
    ));

    let err = eval_err(&mut vm, "(car '(1) '(2))");
    assert!(matches!(
        vm_kind(&err),
        VmErrorKind::WrongArity { got: 2, .. }
    ));
}

#[test]
fn compile_errors_name_their_milestone() {
    let mut vm = vm();

    let err = eval_err(&mut vm, "(let ((x 1)) (lambda (y) (+ x y)))");
    assert!(matches!(
        err,
        RuseError::Compile(ruse::CompileError::CaptureUnsupported { .. })
    ));

    let err = eval_err(&mut vm, "(cond (#t 1))");
    assert!(matches!(
        err,
        RuseError::Compile(ruse::CompileError::Unsupported {
            milestone: "M4",
            ..
        })
    ));

    let err = eval_err(&mut vm, "(lambda args args)");
    assert!(matches!(
        err,
        RuseError::Compile(ruse::CompileError::Unsupported { .. })
    ));

    let err = eval_err(&mut vm, "(define (f) (define x 1) x)");
    assert!(matches!(
        err,
        RuseError::Compile(ruse::CompileError::Unsupported { .. })
    ));
}

#[test]
fn vectors_work_through_the_natives() {
    let mut vm = vm();
    assert_eq!(
        eval(
            &mut vm,
            "(define v (make-vector 3 0)) (vector-set! v 1 42) (vector-ref v 1)"
        ),
        fix(42)
    );
    assert_eq!(eval(&mut vm, "(vector? v)"), Value::TRUE);
    let err = eval_err(&mut vm, "(vector-ref v 9)");
    assert_eq!(
        vm_kind(&err),
        &VmErrorKind::IndexOutOfBounds { index: 9, len: 3 }
    );
}

/// A long-running mutation-heavy loop under a tiny heap trigger would catch a hole in
/// the VM's root set: every live register window must survive every safepoint.
#[test]
fn collections_during_execution_preserve_live_registers() {
    let mut vm = vm();
    // Builds a fresh list per iteration and keeps only the counter — lots of garbage,
    // many safepoints.
    let v = eval(
        &mut vm,
        "(define (churn n acc)
           (if (= n 0)
               acc
               (churn (- n 1) (car (cons (+ acc 1) (list n n n))))))
         (churn 200000 0)",
    );
    assert_eq!(v, fix(200_000));
    assert!(vm.heap().collections() > 0, "the safepoint must have fired");
}
