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

/// Evaluate and print with `write`, so a test can assert on the R7RS spelling of a value.
fn show(vm: &mut Vm, src: &str) -> String {
    let v = eval(vm, src);
    ruse::rt::write::value_to_string(vm.heap(), v, ruse::rt::write::Style::Write)
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
    // Subtracting an exact zero leaves -0.0 alone, inlined or not: the `ADDI` shortcut for
    // a small literal must not turn it into an addition of 0.0.
    assert_eq!(
        eval(
            &mut vm,
            "(define minus -) (define z -0.0) (eqv? (- z 0) (minus z 0))"
        ),
        Value::TRUE
    );
    assert_eq!(eval(&mut vm, "(eqv? (- z 0) -0.0)"), Value::TRUE);
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

    let err = eval_err(&mut vm, "(guard (e (#t 1)) 2)");
    assert!(matches!(
        err,
        RuseError::Compile(ruse::CompileError::Unsupported {
            milestone: "M7",
            ..
        })
    ));

    let err = eval_err(&mut vm, "(define-syntax foo (syntax-rules () ((_) 1)))");
    assert!(matches!(
        err,
        RuseError::Compile(ruse::CompileError::Unsupported {
            milestone: "M8",
            ..
        })
    ));

    // let-values formals that need a real multiple-value producer defer to M7.
    let err = eval_err(&mut vm, "(let-values (((a b) (f))) a)");
    assert!(matches!(
        err,
        RuseError::Compile(ruse::CompileError::Unsupported {
            milestone: "M7",
            ..
        })
    ));
}

// ---------------------------------------------------------------- M4: closures

/// The M4 exit criterion, spec §6.2: two counters from one maker mutate *independent*
/// cells, and `set!` through the closure mutates the binding the closure shares with
/// nothing else.
#[test]
fn make_counter_yields_independent_counters() {
    let mut vm = vm();
    eval(
        &mut vm,
        "(define (make-counter)
           (let ((n 0))
             (lambda () (set! n (+ n 1)) n)))
         (define c1 (make-counter))
         (define c2 (make-counter))",
    );
    assert_eq!(eval(&mut vm, "(c1)"), fix(1));
    assert_eq!(eval(&mut vm, "(c1)"), fix(2));
    assert_eq!(eval(&mut vm, "(c2)"), fix(1));
    assert_eq!(eval(&mut vm, "(c1)"), fix(3));
}

/// Two closures over one binding share one cell: a `set!` through either is visible
/// through the other, before and after the binding's frame has exited.
#[test]
fn closures_over_one_binding_share_the_mutation() {
    let mut vm = vm();
    eval(
        &mut vm,
        "(define p
           (let ((n 0))
             (cons (lambda () (set! n (+ n 1)) n)
                   (lambda () n))))",
    );
    assert_eq!(eval(&mut vm, "((car p))"), fix(1));
    assert_eq!(eval(&mut vm, "((car p))"), fix(2));
    assert_eq!(eval(&mut vm, "((cdr p))"), fix(2));
}

/// A capture whose `let` scope exits mid-function must be closed before the register is
/// reused — the CLOSEUPVALS-at-scope-exit path, as opposed to the frame-exit path.
#[test]
fn a_scope_exit_closes_the_cell_before_the_register_is_reused() {
    let mut vm = vm();
    let v = eval(
        &mut vm,
        "(define (trap)
           (define c (let ((n 10)) (lambda () n)))
           (let ((m 99)) (* m 2))
           (c))
         (trap)",
    );
    assert_eq!(v, fix(10));
}

/// Each iteration of a tail-recursive loop is a separate extent: closures made in
/// different iterations capture different cells (TAILCALL closes before frame reuse).
#[test]
fn loop_iterations_capture_independent_cells() {
    let mut vm = vm();
    let v = eval(
        &mut vm,
        "(define fs
           (let loop ((i 0) (acc '()))
             (if (= i 3) acc (loop (+ i 1) (cons (lambda () i) acc)))))
         (equal? (list ((car fs)) ((car (cdr fs))) ((car (cdr (cdr fs)))))
                 '(2 1 0))",
    );
    assert_eq!(v, Value::TRUE);
}

/// Upvalue chains through more than one lambda level: the middle function passes the
/// capture down as ParentUpval, not a re-capture of a dead register.
#[test]
fn nested_lambdas_capture_through_the_chain() {
    let mut vm = vm();
    let v = eval(
        &mut vm,
        "(define (outer x)
           (lambda ()
             (lambda () (* x 7))))
         (((outer 6)))",
    );
    assert_eq!(v, fix(42));
}

// ---------------------------------------------------------------- M4: binding forms

#[test]
fn variadic_lambdas_collect_rest_arguments() {
    let mut vm = vm();
    assert_eq!(
        eval(
            &mut vm,
            "(define (f . args) args) (equal? (f 1 2 3) '(1 2 3))"
        ),
        Value::TRUE
    );
    assert_eq!(eval(&mut vm, "((lambda args args) )"), Value::NIL);
    assert_eq!(
        eval(
            &mut vm,
            "(define (g a . rest) (cons a rest)) (equal? (g 1 2 3) '(1 2 3))"
        ),
        Value::TRUE
    );
    // Too few required arguments is a typed arity error naming the floor.
    let err = eval_err(&mut vm, "(g)");
    assert!(matches!(
        vm_kind(&err),
        VmErrorKind::WrongArity { got: 0, expected, .. } if expected == "at least 1"
    ));
}

#[test]
fn internal_defines_are_letrec_star() {
    let mut vm = vm();
    // Mutual recursion between internal defines.
    let v = eval(
        &mut vm,
        "(define (classify n)
           (define (even? n) (if (= n 0) #t (odd? (- n 1))))
           (define (odd? n) (if (= n 0) #f (even? (- n 1))))
           (if (even? n) 'even 'odd))
         (classify 9)",
    );
    assert_eq!(v, eval(&mut vm, "'odd"));

    // A same-body forward *value* reference is caught at compile time.
    let err = eval_err(&mut vm, "(define (bad) (define a b) (define b 1) a) (bad)");
    assert!(matches!(
        err,
        RuseError::Compile(ruse::CompileError::PrematureReference { .. })
    ));

    // One routed through a call is caught at run time by the black-hole check.
    let err = eval_err(
        &mut vm,
        "(define (bad2)
           (define f (lambda () g))
           (define x (f))
           (define g 1)
           x)
         (bad2)",
    );
    assert_eq!(vm_kind(&err), &VmErrorKind::UninitializedVariable);
}

#[test]
fn the_let_family_cond_case_and_or_slice() {
    let mut vm = vm();
    assert_eq!(eval(&mut vm, "(let* ((x 2) (y (* x 3))) (+ x y))"), fix(8));
    assert_eq!(
        eval(
            &mut vm,
            "(letrec ((even? (lambda (n) (if (= n 0) #t (odd? (- n 1)))))
                      (odd?  (lambda (n) (if (= n 0) #f (even? (- n 1))))))
               (even? 88))"
        ),
        Value::TRUE
    );
    assert_eq!(
        eval(&mut vm, "(letrec* ((a 2) (b (* a 5))) (+ a b))"),
        fix(12)
    );
    assert_eq!(
        eval(
            &mut vm,
            "(let loop ((i 0) (acc 0)) (if (= i 5) acc (loop (+ i 1) (+ acc i))))"
        ),
        fix(10)
    );
    assert_eq!(
        eval(&mut vm, "(do ((i 0 (+ i 1)) (s 0 (+ s i))) ((= i 5) s))"),
        fix(10)
    );
    assert_eq!(eval(&mut vm, "(and 1 2 3)"), fix(3));
    assert_eq!(eval(&mut vm, "(and 1 #f 3)"), Value::FALSE);
    assert_eq!(eval(&mut vm, "(and)"), Value::TRUE);
    assert_eq!(eval(&mut vm, "(or #f 7 9)"), fix(7));
    assert_eq!(eval(&mut vm, "(or)"), Value::FALSE);
    assert_eq!(eval(&mut vm, "(when (= 1 1) 'a 'b)"), eval(&mut vm, "'b"));
    assert!(eval(&mut vm, "(unless (= 1 1) 'x)").is_unspecified());
    assert_eq!(eval(&mut vm, "(unless (= 1 2) 'x)"), eval(&mut vm, "'x"));
    // cond: plain, arrow, test-only and else clauses.
    assert_eq!(
        eval(&mut vm, "(cond ((= 1 2) 'no) ((= 1 1) 'yes) (else 'else))"),
        eval(&mut vm, "'yes")
    );
    assert_eq!(
        eval(&mut vm, "(cond ((= 1 2) 'no) (else 'else))"),
        eval(&mut vm, "'else")
    );
    assert_eq!(eval(&mut vm, "(cond (#f 'no) ('(7 8) => car))"), fix(7));
    assert_eq!(eval(&mut vm, "(cond (42))"), fix(42));
    assert!(eval(&mut vm, "(cond (#f 1))").is_unspecified());
    // case: eqv? dispatch, else, and both => forms.
    assert_eq!(
        eval(
            &mut vm,
            "(case (* 2 3) ((2 3 5 7) 'prime) ((1 4 6 8 9) 'composite) (else 'other))"
        ),
        eval(&mut vm, "'composite")
    );
    assert_eq!(
        eval(
            &mut vm,
            "(case 9 ((1) 'one) (else => (lambda (k) (+ k 1))))"
        ),
        fix(10)
    );
    assert_eq!(
        eval(&mut vm, "(let-values (((a) 1) ((b) 2)) (+ a b))"),
        fix(3)
    );
    assert_eq!(
        eval(&mut vm, "(let*-values (((a) 1) ((b) (+ a 1))) (+ a b))"),
        fix(3)
    );
    assert_eq!(eval(&mut vm, "(define-values (dv) 11) dv"), fix(11));
}

#[test]
fn quasiquote_builds_structure_with_depth_tracking() {
    let mut vm = vm();
    eval(&mut vm, "(define x 5)");
    assert_eq!(
        eval(&mut vm, "(equal? `(a ,x ,@(list 1 2)) '(a 5 1 2))"),
        Value::TRUE
    );
    // A constant subtree stays a constant datum.
    assert_eq!(eval(&mut vm, "(equal? `(a b (c)) '(a b (c)))"), Value::TRUE);
    // Improper tails and unquoted tails.
    assert_eq!(eval(&mut vm, "(equal? `(1 . ,x) (cons 1 5))"), Value::TRUE);
    // R7RS §4.2.8 nesting: only level-1 unquotes evaluate.
    assert_eq!(
        eval(
            &mut vm,
            "(equal? `(1 `(2 ,(3 ,x))) '(1 (quasiquote (2 (unquote (3 5))))))"
        ),
        Value::TRUE
    );
    // The longhand spellings behave like the sugar.
    assert_eq!(
        eval(&mut vm, "(equal? (quasiquote (a (unquote x))) '(a 5))"),
        Value::TRUE
    );
}

/// Deep recursion through named let and do runs in constant frame space — the loops
/// lower to TAILCALLs of the letrec-bound lambda.
#[test]
fn derived_loops_are_tail_recursive() {
    let mut vm = vm();
    vm.set_frame_limit(50);
    assert_eq!(
        eval(
            &mut vm,
            "(let loop ((i 0)) (if (= i 100000) 'done (loop (+ i 1))))"
        ),
        eval(&mut vm, "'done")
    );
    assert_eq!(
        eval(&mut vm, "(do ((i 0 (+ i 1))) ((= i 100000) 'done))"),
        eval(&mut vm, "'done")
    );
}

/// Safepoint collections with open upvalues on the list and closures in flight: a hole
/// in tracing the open list or the cells is a use-after-free this makes loud.
#[test]
fn collections_preserve_captured_bindings() {
    let mut vm = vm();
    let v = eval(
        &mut vm,
        "(define (churn n)
           (let loop ((i 0) (fs '()))
             (if (= i n)
                 fs
                 (loop (+ i 1) (cons (let ((v (list i i i))) (lambda () (car v))) fs)))))
         (define fs (churn 30000))
         ((car fs))",
    );
    assert_eq!(v, fix(29_999));
    assert!(vm.heap().collections() > 0, "the churn must have collected");
}

/// A closure that escapes (into a global) before its frame dies by *error* must keep
/// the values its registers held at unwind time: `Vm::execute`'s teardown closes every
/// remaining open upvalue rather than dropping the list.
#[test]
fn an_error_unwind_closes_escaped_captures() {
    let mut vm = vm();
    eval(
        &mut vm,
        "(define keep #f)
         (define (boom)
           (let ((n 42))
             (set! keep (lambda () n))
             (car 5)))",
    );
    let err = eval_err(&mut vm, "(boom)");
    assert!(matches!(
        vm_kind(&err),
        VmErrorKind::WrongType { op: "car", .. }
    ));
    // A later execution reuses the register file; the closed cell must not care.
    eval(
        &mut vm,
        "(define (burn n) (if (= n 0) 0 (burn (- n 1)))) (burn 1000)",
    );
    assert_eq!(eval(&mut vm, "(keep)"), fix(42));
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

#[test]
fn numeric_literals_read_compute_and_print() {
    let mut vm = vm();
    assert_eq!(show(&mut vm, "1/3"), "1/3");
    assert_eq!(show(&mut vm, "(+ 1/3 2/3)"), "1");
    assert_eq!(show(&mut vm, "#x10"), "16");
    assert_eq!(show(&mut vm, "#e1.5"), "3/2");
    assert_eq!(show(&mut vm, "+inf.0"), "+inf.0");
    assert_eq!(show(&mut vm, "(* 1+2i 1-2i)"), "5");
    assert_eq!(show(&mut vm, "'(1/2 +i)"), "(1/2 +i)");
    assert_eq!(
        show(&mut vm, "(+ 123456789012345678901234567890 1)"),
        "123456789012345678901234567891"
    );
    assert_eq!(
        eval(&mut vm, "(- 1e400 1e400)")
            .as_flonum()
            .map(f64::is_nan),
        Some(true)
    );
}

#[test]
fn numeric_predicates_and_exactness() {
    let mut vm = vm();
    for (src, expect) in [
        ("(real? -2.5+0i)", true),
        ("(real? -2.5+0.0i)", false),
        ("(rational? 6/10)", true),
        ("(rational? +inf.0)", false),
        ("(integer? 3+0i)", true),
        ("(integer? 3.0)", true),
        ("(exact? #e3.0)", true),
        ("(inexact? 3.)", true),
        ("(exact-integer? 32/5)", false),
        ("(finite? 3.0+inf.0i)", false),
        ("(nan? 1+2i)", false),
        ("(nan? +nan.0)", true),
        ("(positive? +nan.0)", false),
        ("(zero? 0.0+0.0i)", true),
        ("(even? 0)", true),
        ("(odd? -7)", true),
        ("(= 1 1.0 1.0+0.0i)", true),
        ("(= 1.0 1.0+1.0i)", false),
        ("(< +nan.0 0)", false),
        ("(= 9007199254740992.0 9007199254740993)", false),
    ] {
        assert_eq!(eval(&mut vm, src), Value::boolean(expect), "{src}");
    }
    assert_eq!(show(&mut vm, "(exact 0.5)"), "1/2");
    assert_eq!(show(&mut vm, "(exact 1.0)"), "1");
    assert_eq!(show(&mut vm, "(inexact 1/3)"), "0.3333333333333333");
    assert_eq!(show(&mut vm, "(exact->inexact 1)"), "1.0");
    assert_eq!(show(&mut vm, "(inexact->exact 2.5)"), "5/2");
}

#[test]
fn arithmetic_breadth() {
    let mut vm = vm();
    for (src, expect) in [
        ("(/ 1 3)", "1/3"),
        ("(/ 6 4)", "3/2"),
        ("(/ 2)", "1/2"),
        ("(/ 1.0 0.0)", "+inf.0"),
        ("(/ -1073741824 -1)", "1073741824"),
        ("(/ -4611686018427387904 -1)", "4611686018427387904"),
        ("(quotient -4611686018427387904 -1)", "4611686018427387904"),
        ("(max 3.9 4)", "4.0"),
        ("(max 5 3.9 4)", "5.0"),
        ("(min 3 3.1)", "3.0"),
        ("(max 100 +inf.0)", "+inf.0"),
        ("(min -inf.0 -100)", "-inf.0"),
        ("(abs -7/2)", "7/2"),
        ("(remainder -13 -4.0)", "-1.0"),
        ("(modulo -13 4)", "3"),
        ("(floor-quotient 5 -2)", "-3"),
        ("(truncate-remainder -5 2)", "-1"),
        ("(gcd 32 -36)", "4"),
        ("(gcd)", "0"),
        ("(lcm 32.0 -36)", "288.0"),
        ("(lcm)", "1"),
        ("(numerator (/ 6 4))", "3"),
        ("(denominator (inexact (/ 6 4)))", "2.0"),
        ("(numerator 5.5)", "11.0"),
        ("(round 7/2)", "4"),
        ("(round -7/2)", "-4"),
        ("(round 2.5)", "2.0"),
        ("(floor -4.3)", "-5.0"),
        ("(ceiling 3.5)", "4.0"),
        ("(truncate -4.3)", "-4.0"),
        ("(rationalize (exact .3) 1/10)", "1/3"),
        ("(rationalize .3 1/10)", "0.3333333333333333"),
        ("(square 1/2)", "1/4"),
        ("(square -2.0)", "4.0"),
        ("(number->string 255 16)", "\"ff\""),
        ("(number->string 1/3)", "\"1/3\""),
        ("(string->number \"1e2\")", "100.0"),
        ("(string->number \"#e1.5\")", "3/2"),
        ("(string->number \"1 2\")", "#f"),
        ("(string->number \"ff\" 16)", "255"),
        ("(string->number \"#d10\" 16)", "10"),
    ] {
        assert_eq!(show(&mut vm, src), expect, "{src}");
    }
}

#[test]
fn numeric_errors_are_typed() {
    let mut vm = vm();
    assert!(matches!(
        vm_kind(&eval_err(&mut vm, "(/ 1 0)")),
        VmErrorKind::DivisionByZero { op: "/" }
    ));
    assert!(matches!(
        vm_kind(&eval_err(&mut vm, "(/ 1.0 0)")),
        VmErrorKind::DivisionByZero { op: "/" }
    ));
    assert!(matches!(
        vm_kind(&eval_err(&mut vm, "(quotient 5 0)")),
        VmErrorKind::DivisionByZero { op: "quotient" }
    ));
    eval(&mut vm, "(define (f x) (quotient x 0))"); // compiles to QUOT under the licence
    assert!(matches!(
        vm_kind(&eval_err(&mut vm, "(f 5)")),
        VmErrorKind::DivisionByZero { op: "quotient" }
    ));
    assert!(matches!(
        vm_kind(&eval_err(&mut vm, "(exact +inf.0)")),
        VmErrorKind::NumericDomain { op: "exact", .. }
    ));
    assert!(matches!(
        vm_kind(&eval_err(&mut vm, "(number->string 1.5 16)")),
        VmErrorKind::NumericDomain {
            op: "number->string",
            ..
        }
    ));
    assert!(matches!(
        vm_kind(&eval_err(&mut vm, "(number->string 1 3)")),
        VmErrorKind::NumericDomain {
            op: "number->string",
            ..
        }
    ));
    assert!(matches!(
        vm_kind(&eval_err(&mut vm, "(positive? 1+i)")),
        VmErrorKind::NumericDomain {
            op: "positive?",
            ..
        }
    ));
    assert!(matches!(
        vm_kind(&eval_err(&mut vm, "(abs 'a)")),
        VmErrorKind::WrongType { op: "abs", .. }
    ));
    if let RuseError::Vm(e) = eval_err(&mut vm, "(/ 1 0)") {
        assert!(e.span.is_some());
    }
}

#[test]
fn numeric_natives_check_what_the_tower_cannot_see() {
    let mut vm = vm();
    // Type predicates accept any object; the sign and parity tests do not.
    for src in ["(real? 'a)", "(nan? \"x\")", "(exact? #t)"] {
        assert_eq!(eval(&mut vm, src), Value::FALSE, "{src}");
    }
    assert!(matches!(
        vm_kind(&eval_err(&mut vm, "(zero? 'a)")),
        VmErrorKind::WrongType { op: "zero?", .. }
    ));
    assert_eq!(eval(&mut vm, "(even? 2.0)"), Value::TRUE);
    for src in [
        "(odd? 1.5)",
        "(even? 1/2)",
        "(max 1+i)",
        "(gcd 5.5)",
        "(lcm 1/2)",
    ] {
        assert!(
            matches!(
                vm_kind(&eval_err(&mut vm, src)),
                VmErrorKind::NumericDomain { .. }
            ),
            "{src}"
        );
    }
    // A radix outside R7RS's four is an error, not the quiet #f that parsing would give.
    assert!(matches!(
        vm_kind(&eval_err(&mut vm, "(string->number \"10\" 3)")),
        VmErrorKind::NumericDomain {
            op: "string->number",
            ..
        }
    ));
    assert!(matches!(
        vm_kind(&eval_err(&mut vm, "(string->number 10)")),
        VmErrorKind::WrongType {
            op: "string->number",
            ..
        }
    ));
    // Folds start from the first argument: `(/ x)` is a reciprocal, longer calls divide
    // left to right, and a lone argument is still checked.
    assert_eq!(show(&mut vm, "(/ 1 2 4)"), "1/8");
    assert_eq!(show(&mut vm, "(/ 0.0)"), "+inf.0");
    assert!(matches!(
        vm_kind(&eval_err(&mut vm, "(/ 0)")),
        VmErrorKind::DivisionByZero { op: "/" }
    ));
    assert_eq!(show(&mut vm, "(gcd -4)"), "4");
    assert_eq!(show(&mut vm, "(max 2/3)"), "2/3");
}

#[test]
fn transcendental_and_complex_procedures() {
    let mut vm = vm();
    for (src, expect) in [
        ("(sqrt -4)", "+2i"),
        ("(inexact (sqrt -1))", "0.0+1.0i"),
        ("(expt 2 100)", "1267650600228229401496703205376"),
        ("(expt 0.0 0)", "1.0"),
        ("(exp 0)", "1.0"),
        ("(log 8 2)", "3.0"),
        ("(atan -0.0 1.0)", "-0.0"),
        ("(make-rectangular 1 2)", "1+2i"),
        ("(make-rectangular 1 2.0)", "1.0+2.0i"),
        ("(make-polar 1 0)", "1"),
        ("(real-part 1+2i)", "1"),
        ("(imag-part 3)", "0"),
        ("(magnitude 3+4i)", "5"),
        ("(angle -1)", "3.141592653589793"),
    ] {
        assert_eq!(show(&mut vm, src), expect, "{src}");
    }
    assert!(matches!(
        vm_kind(&eval_err(&mut vm, "(expt 2 4294967296)")),
        VmErrorKind::NumericDomain { op: "expt", .. }
    ));
}
