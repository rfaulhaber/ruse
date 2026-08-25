//! Snapshot tests for the disassembler: the frozen mnemonic-text surface.
//!
//! The three worked examples are the spec §6 compilations, *derived* rather than
//! transcribed — the spec's own listings were schematic (constants in register operands,
//! approximate offsets) and are amended to match what this file freezes. Everything here
//! passes the verifier first, so the snapshots only ever show well-formed code; the
//! robustness listings at the bottom show how malformed input prints without verifying it.
//!
//! Tests assert on disassembly text and never on raw instruction words, so renumbering the
//! opcode byte table is a snapshot review, not a test rewrite (frozen decision, see
//! `docs/project_plan.org`).

// Integration tests are separate crates, so the crate-root `cfg_attr(test, allow(...))` in
// the library does not reach them. Asserting with `unwrap` is the point of a test.
#![allow(clippy::unwrap_used)]

use std::rc::Rc;

use ruse::bytecode::{Insn, Op, Proto, UpvalDesc, verify};
use ruse::disasm::disassemble;
use ruse::{Heap, Value};

fn fixnum(n: i64) -> Value {
    Value::fixnum(n).unwrap()
}

/// Spec §6.1 — tail-recursive factorial:
///
/// ```scheme
/// (define (fact n acc)
///   (if (= n 0) acc (fact (- n 1) (* n acc))))
/// ```
///
/// Derivation notes, versus the spec's schematic listing: `NUMEQ` cannot take a constant
/// operand, so `0` is loaded into a register first; the `k` flag is `0` so the `JMP` to the
/// recursive branch executes when `(= n 0)` is false; `(- n 1)` is `ADDI -1`; and the tail
/// call's callee and arguments are laid out contiguously in `r2..r4`.
fn fact(heap: &mut Heap) -> Proto {
    let k_fact = heap.symbol("fact");
    Proto {
        name: Some("fact".to_string()),
        nparams: 2, // r0 = n, r1 = acc
        max_window: 5,
        consts: vec![fixnum(0), k_fact],
        code: vec![
            Insn::iabx(Op::LoadK, 2, 0),              // r2 := 0
            Insn::iabc(Op::NumEq, 0, 2, 0),           // (= n 0)? false -> take the JMP
            Insn::iasbx(Op::Jmp, 0, 1),               // -> recur
            Insn::iabc(Op::Return1, 1, 0, 0),         // n = 0: return acc
            Insn::iabx(Op::GetGlobal, 2, 1),          // r2 := fact
            Insn::iabc(Op::AddI, 3, 0, (-1i8) as u8), // r3 := n - 1
            Insn::iabc(Op::Mul, 4, 0, 1),             // r4 := n * acc
            Insn::iabc(Op::TailCall, 2, 3, 0),        // fact(n-1, n*acc), frame reused
        ],
        ..Proto::default()
    }
}

#[test]
fn spec_6_1_factorial() {
    let mut heap = Heap::new();
    let proto = fact(&mut heap);
    assert_eq!(verify(&proto), Ok(()));
    insta::assert_snapshot!(disassemble(&heap, &proto));
}

/// Spec §6.2 — a closure capturing a variable:
///
/// ```scheme
/// (define (make-counter)
///   (let ((n 0))
///     (lambda () (set! n (+ n 1)) n)))
/// ```
///
/// The capture is described by the child's `UpvalDesc` table (`u0 <- parent local r0`),
/// not by pseudo-instructions after `CLOSURE`; `CLOSEUPVALS 0` moves `n` into its heap
/// cell when `make-counter` returns, which is what makes two counters independent.
fn make_counter() -> Proto {
    let inner = Proto {
        max_window: 1,
        upvals: vec![UpvalDesc::ParentLocal(0)],
        code: vec![
            Insn::iabc(Op::GetUpval, 0, 0, 0), // r0 := n
            Insn::iabc(Op::AddI, 0, 0, 1),     // r0 := n + 1
            Insn::iabc(Op::SetUpval, 0, 0, 0), // n := r0, through the shared cell
            Insn::iabc(Op::Return1, 0, 0, 0),
        ],
        ..Proto::default()
    };
    Proto {
        name: Some("make-counter".to_string()),
        max_window: 2,
        protos: vec![Rc::new(inner)],
        code: vec![
            Insn::iasbx(Op::LoadI, 0, 0),         // r0 := n := 0
            Insn::iabx(Op::Closure, 1, 0),        // r1 := closure over p0
            Insn::iabc(Op::CloseUpvals, 0, 0, 0), // n escapes: close it into its cell
            Insn::iabc(Op::Return1, 1, 0, 0),
        ],
        ..Proto::default()
    }
}

#[test]
fn spec_6_2_make_counter() {
    let heap = Heap::new();
    let proto = make_counter();
    assert_eq!(verify(&proto), Ok(()));
    insta::assert_snapshot!(disassemble(&heap, &proto));
}

/// Spec §6.3 — a call/cc escape:
///
/// ```scheme
/// (+ 1 (call/cc (lambda (k) (k 10))))
/// ```
///
/// `CAPTURECC` writes the continuation straight into the call's argument slot, and inside
/// the lambda `(k 10)` is in tail position, so invoking the continuation is a `TAILCALL`
/// of an ordinary callable.
fn call_cc_escape() -> Proto {
    let lambda = Proto {
        nparams: 1, // r0 = k
        max_window: 2,
        code: vec![
            Insn::iasbx(Op::LoadI, 1, 10),
            Insn::iabc(Op::TailCall, 0, 2, 0), // (k 10)
        ],
        ..Proto::default()
    };
    Proto {
        max_window: 4,
        protos: vec![Rc::new(lambda)],
        code: vec![
            Insn::iasbx(Op::LoadI, 1, 1),       // r1 := 1
            Insn::iabx(Op::Closure, 2, 0),      // r2 := (lambda (k) (k 10))
            Insn::iabc(Op::CaptureCc, 3, 0, 0), // r3 := the current continuation
            Insn::iabc(Op::Call, 2, 2, 2),      // call the lambda with k
            Insn::iabc(Op::Add, 0, 1, 2),       // r0 := 1 + result
            Insn::iabc(Op::Return1, 0, 0, 0),
        ],
        ..Proto::default()
    }
}

#[test]
fn spec_6_3_call_cc_escape() {
    let heap = Heap::new();
    let proto = call_cc_escape();
    assert_eq!(verify(&proto), Ok(()));
    insta::assert_snapshot!(disassemble(&heap, &proto));
}

/// Every opcode prints, with every constant-rendering case. The listing follows the §5
/// byte order except `EXTRAARG`, which must sit beside its `LOADKX`. It is deliberately
/// *not* verified (it is not a program), but every index it uses is in bounds so the
/// text shows the good-path rendering; the malformed listing below shows the annotations.
#[test]
fn every_opcode_renders() {
    let mut heap = Heap::new();
    let string = heap.string("hi\n");
    let symbol = heap.symbol("foo");
    let big = heap.integer(i64::MAX); // past FIXNUM_MAX, so a heap bignum
    let pair = heap.cons(Value::NIL, Value::NIL);

    let child = Proto {
        name: Some("kid".to_string()),
        max_window: 1,
        code: vec![Insn::iabc(Op::Return1, 0, 0, 0)],
        ..Proto::default()
    };
    let proto = Proto {
        name: Some("all-ops".to_string()),
        max_window: 3,
        consts: vec![
            string,
            Value::flonum(2.5),
            symbol,
            Value::char('λ'),
            big,
            pair,
            Value::TRUE,
            fixnum(-3),
        ],
        protos: vec![Rc::new(child)],
        upvals: vec![UpvalDesc::ParentLocal(0), UpvalDesc::ParentUpval(0)],
        code: vec![
            Insn::iabc(Op::Move, 1, 0, 0),
            Insn::iabx(Op::LoadK, 0, 0),
            Insn::iabx(Op::LoadKx, 0, 0),
            Insn::iax(Op::ExtraArg, 1),
            Insn::iabx(Op::LoadImm, 0, 4),
            Insn::iasbx(Op::LoadI, 0, -7),
            Insn::iabc(Op::Add, 2, 0, 1),
            Insn::iabc(Op::Sub, 2, 0, 1),
            Insn::iabc(Op::Mul, 2, 0, 1),
            Insn::iabc(Op::Div, 2, 0, 1),
            Insn::iabc(Op::Quot, 2, 0, 1),
            Insn::iabc(Op::Neg, 1, 0, 0),
            Insn::iabc(Op::AddI, 1, 0, (-1i8) as u8),
            Insn::iabc(Op::NumEq, 0, 1, 1),
            Insn::iabc(Op::NumLt, 0, 1, 0),
            Insn::iabc(Op::NumLe, 0, 1, 1),
            Insn::iabc(Op::Eq, 0, 1, 0),
            Insn::iabc(Op::Eqv, 0, 1, 1),
            Insn::iabc(Op::Test, 0, 0, 1),
            Insn::iasbx(Op::Jmp, 0, 2),
            Insn::iasbx(Op::Jmp, 2, -3),
            Insn::iabc(Op::JmpIdx, 0, 1, 2),
            Insn::iabc(Op::Cons, 2, 0, 1),
            Insn::iabc(Op::Car, 1, 0, 0),
            Insn::iabc(Op::Cdr, 1, 0, 0),
            Insn::iabc(Op::SetCar, 0, 1, 0),
            Insn::iabc(Op::SetCdr, 0, 1, 0),
            Insn::iabc(Op::Cadr, 1, 0, 6),
            Insn::iabc(Op::TypeP, 0, 0, 3),
            Insn::iabc(Op::Call, 0, 0, 0),
            Insn::iabc(Op::Call, 0, 3, 2),
            Insn::iabc(Op::TailCall, 0, 1, 0),
            Insn::iabc(Op::Return, 0, 0, 0),
            Insn::iabc(Op::Return, 0, 1, 0),
            Insn::iabc(Op::Return, 0, 3, 0),
            Insn::iabc(Op::Return1, 0, 0, 0),
            Insn::iabc(Op::Apply, 0, 3, 0),
            Insn::iabx(Op::Closure, 0, 0),
            Insn::iabc(Op::GetUpval, 0, 1, 0),
            Insn::iabc(Op::SetUpval, 0, 1, 0),
            Insn::iabx(Op::GetGlobal, 0, 2),
            Insn::iabx(Op::SetGlobal, 0, 2),
            Insn::iabc(Op::GetLocalN, 1, 0, 0),
            Insn::iabc(Op::CloseUpvals, 1, 0, 0),
            Insn::iabc(Op::VecRef, 2, 0, 1),
            Insn::iabc(Op::VecSet, 0, 1, 2),
            Insn::iabc(Op::NewVec, 2, 0, 1),
            Insn::iabc(Op::PrimCall, 0, 2, 7),
            Insn::iabc(Op::CaptureCc, 2, 0, 0),
            Insn::iabc(Op::WindPush, 0, 1, 0),
            Insn::iabc(Op::WindPop, 0, 0, 0),
            Insn::iasbx(Op::HandlerPush, 0, 3),
            Insn::iabc(Op::HandlerPop, 0, 0, 0),
            Insn::iabc(Op::Raise, 0, 1, 0),
            Insn::iabc(Op::Raise, 0, 0, 0),
            // LOADK for each remaining constant-rendering case.
            Insn::iabx(Op::LoadK, 0, 3),
            Insn::iabx(Op::LoadK, 0, 4),
            Insn::iabx(Op::LoadK, 0, 5),
            Insn::iabx(Op::LoadK, 0, 6),
            Insn::iabx(Op::LoadK, 0, 7),
            // A reserved byte from the 0x1E gap and one from the RBC-2 block.
            Insn(0x0000_001E),
            Insn(0x0201_0040),
        ],
        ..Proto::default()
    };

    // "Every opcode" is enforced, not aspirational: a 51st opcode without a listing line
    // fails here before the snapshot can silently omit it.
    let rendered: std::collections::HashSet<u8> = proto
        .code
        .iter()
        .filter_map(|insn| insn.opcode().map(|op| op as u8))
        .collect();
    assert_eq!(rendered.len(), Op::COUNT, "listing must cover all opcodes");

    insta::assert_snapshot!(disassemble(&heap, &proto));
}

/// Malformed input prints, annotated, instead of panicking: the disassembler is the tool
/// you reach for exactly when a prototype is wrong.
#[test]
fn malformed_prototypes_still_print() {
    let heap = Heap::new();
    let proto = Proto {
        name: Some("broken".to_string()),
        max_window: 1,
        code: vec![
            Insn::iabx(Op::LoadK, 0, 9),       // constant out of bounds
            Insn::iabx(Op::LoadKx, 0, 0),      // no EXTRAARG follows
            Insn::iabx(Op::LoadImm, 0, 9),     // not a singleton ordinal
            Insn::iasbx(Op::Jmp, 0, 99),       // target out of bounds
            Insn::iabx(Op::Closure, 0, 4),     // child out of bounds
            Insn::iabc(Op::GetUpval, 0, 9, 0), // upvalue out of bounds
        ],
        ..Proto::default()
    };
    assert!(verify(&proto).is_err());
    insta::assert_snapshot!(disassemble(&heap, &proto));
}

/// The exit criterion's round trip: decode every instruction back to fields through the
/// spec §3.1 accessors, re-encode those fields through the format constructors, and the
/// result must be the identical program — verified and disassembling to the same text.
#[test]
fn disassembly_is_stable_across_encode_decode_round_trips() {
    use ruse::bytecode::Format;

    let mut heap = Heap::new();
    let proto = fact(&mut heap);
    let first = disassemble(&heap, &proto);

    let reencode = |insn: &Insn| {
        let op = insn.opcode().unwrap();
        match op.format() {
            Format::Abc => Insn::iabc(op, insn.a(), insn.b(), insn.c()),
            Format::Abx => Insn::iabx(op, insn.a(), insn.bx()),
            Format::Asbx => Insn::iasbx(op, insn.a(), insn.sbx()),
            Format::Ax => Insn::iax(op, insn.ax()),
        }
    };
    let reencoded = Proto {
        name: proto.name.clone(),
        code: proto.code.iter().map(reencode).collect(),
        consts: proto.consts.clone(),
        nparams: proto.nparams,
        max_window: proto.max_window,
        ..Proto::default()
    };

    assert_eq!(
        reencoded.code, proto.code,
        "re-encoding must be the identity"
    );
    assert_eq!(verify(&reencoded), Ok(()));
    assert_eq!(first, disassemble(&heap, &reencoded));
}
