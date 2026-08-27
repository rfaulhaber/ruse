//! Snapshot tests for the compiler: source in, disassembly text out.
//!
//! Per the frozen testing doctrine, compiled output is asserted as the disassembler's
//! mnemonic text, never as raw instruction words — so the opcode byte table stays cheap
//! to renumber and a codegen regression reads as a legible diff. Every prototype shown
//! here passed the load-time verifier on its way through `compile_only`'s caller path;
//! the separate `verified` test below pins that explicitly for each snippet.

// Integration tests are separate crates, so the crate-root `cfg_attr(test, allow(...))` in
// the library does not reach them. Asserting with `unwrap` is the point of a test.
#![allow(clippy::unwrap_used)]

use ruse::bytecode::verify;
use ruse::disasm::disassemble;
use ruse::{Parser, Vm};

/// Every snapshotted snippet, in one place so the verifier test walks the same list.
const SNIPPETS: &[(&str, &str)] = &[
    (
        "fact_tail_recursive",
        "(define (fact n acc) (if (= n 0) acc (fact (- n 1) (* n acc))))",
    ),
    ("comparison_as_value", "(< (car '(1 2)) 3)"),
    (
        "if_with_else_branches",
        "(if (> 2 1) (display \"yes\") (display \"no\"))",
    ),
    (
        "let_with_local_set",
        "(let ((x 1) (y 2)) (set! x (+ x y)) x)",
    ),
    ("begin_sequences_effects", "(begin (display 1) 2)"),
    (
        "quoted_datum_pool",
        "'(1 2.5 \"s\" #\\c #t (nested . pair))",
    ),
    ("addi_peephole", "(define (inc n) (+ n 1))"),
    ("arith_fold", "(+ 1 2 3 4)"),
    ("primcall_with_result", "(equal? (list 1 2) '(1 2))"),
    ("tail_primcall_returns", "(define (say x) (display x))"),
    ("higher_order_heads_stay_calls", "((if #f + *) 3 4)"),
    ("nested_lambdas", "(lambda (x) (lambda (y) 7))"),
    ("set_global_probes_boundness", "(set! x 1)"),
    // The spec §6.2 exit-criterion program: a capture descriptor on the child, GETUPVAL/
    // SETUPVAL through the cell, and the let-scope close before the fall-through return.
    (
        "make_counter_closure",
        "(define (make-counter) (let ((n 0)) (lambda () (set! n (+ n 1)) n)))",
    ),
    // A grandchild reaches the outer binding through the middle lambda's own upvalue:
    // ParentLocal in the middle prototype, ParentUpval in the innermost.
    (
        "capture_through_two_levels",
        "(lambda (x) (lambda () (lambda () x)))",
    ),
    ("variadic_rest_param", "(define (f a . rest) rest)"),
    (
        "internal_defines_are_letrec",
        "(define (h x) (define y (* x 2)) (define (helper n) (+ n y)) (helper 10))",
    ),
    (
        "named_let_compiles_to_tailcall_loop",
        "(let loop ((i 0)) (if (= i 9) i (loop (+ i 1))))",
    ),
    (
        "cond_with_arrow",
        "(cond ((= 1 2) 'no) ('(7) => car) (else 'else))",
    ),
    ("case_dispatches_on_eqv", "(case 3 ((1 2) 'lo) ((3 4) 'hi))"),
    ("and_or_when", "(and 1 (or #f 2) (when #t 3))"),
    ("quasiquote_splice", "`(a ,x ,@(list 1 2))"),
];

fn compile_text(vm: &mut Vm, src: &str) -> String {
    let exprs = Parser::parse_from_str(src).unwrap();
    let proto = vm.compile_only(&exprs[0]).unwrap();
    assert_eq!(verify(&proto), Ok(()), "compiled output must verify: {src}");
    disassemble(vm.heap(), &proto)
}

#[test]
fn snapshots() {
    for &(name, src) in SNIPPETS {
        let mut vm = Vm::with_output(Box::new(Vec::new()));
        insta::assert_snapshot!(name, compile_text(&mut vm, src));
    }
}

/// The inlining licence: redefining `+` makes later forms compile it as an honest
/// global call instead of ADD.
#[test]
fn redefinition_revokes_the_inline_licence() {
    let mut vm = Vm::with_output(Box::new(Vec::new()));
    insta::assert_snapshot!("plus_pristine", compile_text(&mut vm, "(+ 10 1)"));
    vm.eval_str("(define + -)").unwrap();
    insta::assert_snapshot!("plus_redefined", compile_text(&mut vm, "(+ 10 1)"));
}

/// A locally bound name shadows both keywords and primitives.
#[test]
fn lexical_shadowing_compiles_to_calls() {
    let mut vm = Vm::with_output(Box::new(Vec::new()));
    insta::assert_snapshot!(
        "shadowed_keyword_and_prim",
        compile_text(&mut vm, "(let ((if car) (car cdr)) (if (car '(1 2))))")
    );
}
