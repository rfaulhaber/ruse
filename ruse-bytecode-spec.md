# Ruse Bytecode Specification (RBC-1)

**Version 0.1.0 — DRAFT**
**Target:** R7RS Scheme on a register-based VM, implemented in Rust
**Companion to:** RUVM Specification v0.1.0

---

## 1. Design Philosophy

This document specifies the instruction set architecture (ISA) for the Ruse VM. The design is Lua-inspired — fixed-width 32-bit instructions, a flat register file with per-call register windows, skip-style fused comparisons — but it diverges from Lua wherever Scheme's semantics or performance profile demand it.

### 1.1 Guiding principles

1. **Hot paths get opcodes; cold paths get runtime calls.** Profiled Scheme workloads spend their time in: function calls, pair traversal (`car`/`cdr`), fixnum arithmetic, type dispatch, and closure variable access. Every one of these is a dedicated opcode with an inline fast path. Conversely, `string-upcase`, `assoc`, bignum arithmetic, and port I/O go through the native-function mechanism — adding opcodes for them would bloat the dispatch loop's instruction cache footprint without measurable gain.

2. **Only `#f` is false.** Scheme's truthiness is trivial compared to Lua's (`nil` and `false`). The `TEST` family compares against a single immediate bit pattern — one branch, no type dispatch.

3. **Fixnum fast path, tower slow path.** Arithmetic opcodes check both operands for the fixnum tag (a single mask-and-compare under NaN-boxing), execute inline if both match, and otherwise call out to the numeric tower runtime (bignums, rationals, flonums, complex). The bytecode never encodes numeric representation — that is a runtime property of the values.

4. **Multiple values are first-class in the calling convention, not bolted on.** `values` and `call-with-values` compile to ordinary `RETURN`/`CALL` instructions using open-ended operand counts (the Lua `B=0`/`C=0` trick). No separate MV register file, no MV opcodes.

5. **Continuations and tail calls are non-negotiable.** `TAILCALL` reuses the current frame unconditionally — proper tail calls are a correctness requirement (R7RS §3.5), not an optimization. `CAPTURECC` snapshots frames; continuations are *callable objects*, so invoking one is just `CALL`.

6. **The compiler is smart so the VM can be dumb.** No immediate-operand comparison opcodes: a register VM lets the compiler hoist loop constants into registers once, outside the loop. The single exception is `ADDI`, because increment-by-constant is ubiquitous and saving a register in tight loops measurably reduces window pressure.

### 1.2 Known performance traps (flagged, not solved here)

- **Exact division:** `(/ 1 3)` must produce the exact rational `1/3` (R7RS §6.2.6). `DIV` therefore *cannot* have a fixnum-only fast path the way `ADD` can — fixnum ÷ fixnum produces a rational unless the division is exact. The fast path inside `DIV` is "divides evenly"; everything else allocates. Scheme code that wants machine-speed division should use `QUOT` (`truncate/`) or flonums. This is inherent to the language, not a design flaw, but it is worth knowing before benchmarking.
- **Generic arithmetic dispatch:** even the fixnum fast path costs a tag check per operand. A future flonum-specialized opcode tier (`FADD`, `FMUL`, …) driven by compiler type inference is the natural next step, but is **out of scope for RBC-1**. Get correct first.

---

## 2. Execution Model (summary)

Full detail lives in the RUVM spec; this section fixes only what the ISA depends on.

- **Values** are 64-bit NaN-boxed words: f64 flonums immediate, 48-bit payload fixnums, tagged heap pointers, and immediate singletons (`#t`, `#f`, `'()`, eof-object, *unspecified*, *undefined*) plus characters.
- **Registers**: each call frame owns a window of up to **250** registers (`R0`–`R249`) within a flat per-fiber register array. 8-bit operand fields address registers 0–255; values 250–255 are reserved for future addressing-mode escapes.
- **Frames**: a frame records the callee closure, return address, window base, expected result count, and `fiber_id`.
- **Calling convention** (Lua-style): for `CALL A B C`, the callee sits in `R[A]`, arguments in `R[A+1] … R[A+B-1]`. On entry the callee's window is rebased so its `R0` aliases the caller's `R[A+1]` — argument passing is free.
- **Prototypes** (compiled functions) carry: the instruction array, constant pool, upvalue descriptors, child prototypes, arity metadata (`nparams`, `has_rest`), max window size, and a line-info side table. **Rest-argument collection is driven by prototype metadata at call entry, not by an opcode** — the VM conses extra arguments into a list in `R[nparams]` when `has_rest` is set.
- **Per-fiber stacks**: the wind stack (dynamic-wind), handler stack (exceptions), and frame stack are fiber-local. `CAPTURECC` snapshots all three.

---

## 3. Instruction Encoding

All instructions are 32 bits, stored little-endian. The opcode occupies the low 8 bits, giving 256 opcode slots (RBC-1 defines 50; see §5 for the frozen assignment). Four formats:

```
 31              24 23              16 15               8 7                0
┌──────────────────┬──────────────────┬──────────────────┬──────────────────┐
│        C : 8     │        B : 8     │        A : 8     │      OP : 8      │  iABC
├──────────────────┴──────────────────┼──────────────────┼──────────────────┤
│              Bx : 16 (unsigned)     │        A : 8     │      OP : 8      │  iABx
├─────────────────────────────────────┼──────────────────┼──────────────────┤
│             sBx : 16 (signed)       │        A : 8     │      OP : 8      │  iAsBx
├─────────────────────────────────────┴──────────────────┼──────────────────┤
│                      Ax : 24 (unsigned)                │      OP : 8      │  iAx
└─────────────────────────────────────────────────────────┴─────────────────┘
```

- **iABC** — three register operands, or registers plus a small immediate/flag.
- **iABx** — register + 16-bit unsigned index (constant pool, global slots, prototype index). Pools larger than 65,536 entries use `LOADKX` + `EXTRAARG`.
- **iAsBx** — register + 16-bit signed offset (jumps, small integer literals). `sBx` is two's complement; jump range is ±32,767 instructions. The compiler is responsible for splitting functions that exceed this (in practice, never).
- **iAx** — single 24-bit operand, used only by `EXTRAARG`.

Some iABC instructions interpret a field as a **signed** byte (written `sC`) or as a **flag** (written `k`, value 0 or 1). This is noted per-instruction. Two conventions are frozen across the set: the skip-family flag `k` always lives in field **C** (including `TEST`), and any field an instruction does not use must encode as **zero** — every instruction has exactly one canonical encoding, which the load-time verifier enforces.

### 3.1 Rust representation

```rust
#[derive(Clone, Copy, PartialEq, Eq)]
#[repr(transparent)]
pub struct Insn(pub u32);

impl Insn {
    #[inline(always)] pub fn op(self)  -> u8  { (self.0 & 0xFF) as u8 }
    #[inline(always)] pub fn a(self)   -> u8  { ((self.0 >> 8) & 0xFF) as u8 }
    #[inline(always)] pub fn b(self)   -> u8  { ((self.0 >> 16) & 0xFF) as u8 }
    #[inline(always)] pub fn c(self)   -> u8  { ((self.0 >> 24) & 0xFF) as u8 }
    #[inline(always)] pub fn sc(self)  -> i8  { self.c() as i8 }
    #[inline(always)] pub fn bx(self)  -> u16 { (self.0 >> 16) as u16 }
    #[inline(always)] pub fn sbx(self) -> i16 { (self.0 >> 16) as u16 as i16 }
    #[inline(always)] pub fn ax(self)  -> u32 { self.0 >> 8 }
}
```

The dispatch loop is a `loop { match insn.op() { … } }`. Stable Rust cannot yet guarantee computed-goto/tail-call dispatch; a well-shaped match on a dense `u8` compiles to a jump table, which is acceptable for RBC-1. Revisit with `become` (explicit tail calls) when stabilized.

The canonical implementation is `src/bytecode/insn.rs`, which pairs these accessors with the symmetric encoders `iabc`/`iabx`/`iasbx`/`iax`.

### 3.2 The skip-next convention

Comparison instructions do not produce boolean values or carry jump targets. Instead, following Lua: a comparison evaluates its condition against the `k` flag, and **if the condition ≠ k, the next instruction is skipped**. The next instruction is, by compiler convention, always a `JMP`. This keeps comparisons in iABC format (two full register operands) while allowing 16-bit jump ranges, at the cost of two instruction slots per branch — a good trade, since the not-taken path falls through in one dispatch.

When Scheme source needs a comparison *as a value* — `(define x (< a b))` — the compiler synthesizes it:

```
NUMLT   r1, r2, 1      ; if (< r1 r2), fall into the JMP; otherwise skip it
JMP     +2             ; -> the #t branch
LOADIMM r0, #f
JMP     +1             ; skip the #t branch
LOADIMM r0, #t
```

(or the shorter `LOADIMM`/skip pattern; codegen's choice). This is rarer than branching in real code, which is why branching gets the fast encoding.

---

## 4. The Instruction Set

50 opcodes in ten functional groups. Notation: `R[x]` is register x in the current window; `K[x]` is constant-pool entry x; `U[x]` is upvalue x of the running closure; `G[x]` is the global slot named by `K[x]`; `PC` is the program counter (points at the *next* instruction). `RA`/`RB`/`RC` abbreviate `R[A]`/`R[B]`/`R[C]`.

### 4.1 Data movement (5)

| Op | Format | Effect |
|----|--------|--------|
| `MOVE A B` | iABC | `RA := RB`. Register-to-register copy. |
| `LOADK A Bx` | iABx | `RA := K[Bx]`. Load constant. |
| `LOADKX A` | iABx | `RA := K[Ax]` where Ax comes from a following `EXTRAARG`. For >64K constant pools. |
| `LOADIMM A Bx` | iABx | `RA :=` immediate singleton selected by Bx. The operand **is** the singleton ordinal defined in `src/value/layout.rs` (`0=undefined 1=unspecified 2='() 3=eof 4=#f 5=#t`) — that file is the single source of truth, and there is no second table to drift from it. No constant-pool slot needed for these. |
| `LOADI A sBx` | iAsBx | `RA :=` the fixnum `sBx`. Small integer literals (−32768..32767) without a pool entry. |

`undefined` (slot 5) is the letrec "black hole" — bound but not yet initialized. Referencing it raises an error; this is how RBC-1 detects `(letrec ((x (+ x 1))) x)` at runtime.

### 4.2 Arithmetic (7)

All check operands for the fixnum tag and execute inline on the fast path; otherwise dispatch to the numeric-tower runtime, which handles flonum/bignum/rational/complex and signals a non-number error. Fixnum overflow promotes to bignum (never wraps — R7RS integers are unbounded).

| Op | Format | Effect |
|----|--------|--------|
| `ADD A B C` | iABC | `RA := RB + RC` |
| `SUB A B C` | iABC | `RA := RB − RC` |
| `MUL A B C` | iABC | `RA := RB × RC` |
| `DIV A B C` | iABC | `RA := RB ÷ RC`. **Exact division** — see §1.2. Fast path only when fixnum RC divides fixnum RB evenly; otherwise allocates a rational or signals divide-by-zero. |
| `QUOT A B C` | iABC | `RA := (truncate/ RB RC)` quotient. Fixnum-fast, integer-only, the "machine division" escape hatch. |
| `NEG A B` | iABC | `RA := −RB` |
| `ADDI A B sC` | iABC | `RA := RB +` (signed byte) `sC`. Loop-counter fast path; sC ∈ −128..127. |

`MOD`/`REM` and the full division family (`floor/`, `round/`, etc.) are runtime calls, not opcodes — they're rare enough relative to `+`/`−`/`×` that the dispatch-table slot isn't worth it.

### 4.3 Comparison (6) — skip-next convention (§3.2)

| Op | Format | Effect |
|----|--------|--------|
| `NUMEQ A B k` | iABC | if `(= RA RB) ≠ k` then `PC++` (skip). Numeric `=`. |
| `NUMLT A B k` | iABC | if `(< RA RB) ≠ k` then skip. |
| `NUMLE A B k` | iABC | if `(<= RA RB) ≠ k` then skip. |
| `EQ A B k`    | iABC | if `(eq? RA RB) ≠ k` then skip. Pointer/immediate identity. |
| `EQV A B k`   | iABC | if `(eqv? RA RB) ≠ k` then skip. eq? plus number/char value equality. |
| `TEST A k`    | iABC | if `(RA is truthy) ≠ k` then skip. The `if`/`and`/`or`/`when`/`cond` primitive. Only `#f` is false. |

`>` and `>=` are compiled by swapping operands into `NUMLT`/`NUMLE`. `equal?` (deep, cyclic-safe) is a runtime call. `NUMEQ`/`NUMLT`/`NUMLE` fixnum-fast, tower-slow like the arithmetic ops.

### 4.4 Control flow (3)

| Op | Format | Effect |
|----|--------|--------|
| `JMP A sBx` | iAsBx | `PC += sBx`. If `A > 0`, also close all open upvalues at and above register `A−1` (loop-exit upvalue closing, Lua-style). `A = 0` means "no upvalues to close." |
| `LOADKX`-pair `EXTRAARG Ax` | iAx | Carries a 24-bit operand for the immediately preceding instruction. Never executed alone. |
| `JMPIDX A B C` | iABC | Computed jump: `PC += RB` if `RA < RC`, else fall through. Compiles `case` dispatch tables to O(1). |

`JMPIDX` is the one structural concession to a common Scheme form: large `case` expressions over small-integer or char keys become a jump table instead of a comparison chain. It is optional codegen — `case` can also lower to `EQV` chains — but having the opcode lets the compiler choose.

### 4.5 Pairs and lists (6)

The traversal hot path. Each fast-paths on the pair tag and signals a wrong-type error otherwise.

| Op | Format | Effect |
|----|--------|--------|
| `CONS A B C` | iABC | `RA := (cons RB RC)`. Allocates. |
| `CAR A B` | iABC | `RA := (car RB)`. Type-checked. |
| `CDR A B` | iABC | `RA := (cdr RB)`. Type-checked. |
| `SETCAR A B` | iABC | `(set-car! RA RB)`. Write barrier for GC. |
| `SETCDR A B` | iABC | `(set-cdr! RA RB)`. Write barrier for GC. |
| `CADR A B C` | iABC | `RA := nth-cdr-then-car (RB)`, path encoded in C's low bits (1=a 0=d, LSB first, length in high nibble). Fuses `caar`…`cddddr` into one instruction. |

`CADR` is worth its slot because `(cadr x)`, `(cddr x)`, `(caddr x)` saturate list-processing code; fusing avoids both the intermediate register and the repeated tag check.

### 4.6 Type predicates (1, fused)

| Op | Format | Effect |
|----|--------|--------|
| `TYPEP A B C` | iABC | Skip-next: if `(type-of RB == C) ≠ k`... — wait, k is needed. |

> **Correction:** `TYPEP` needs both a type selector and the skip flag, which overflows iABC's three byte-fields plus a flag. Resolution: the type selector goes in **C** (an enum: 0=pair? 1=null? 2=number? 3=integer? 4=symbol? 5=string? 6=char? 7=vector? 8=procedure? 9=boolean? 10=eof? …), and the skip is *always against true* — `TYPEP A _ C` skips the next instruction unless `RA` has type `C`. Negated predicates put the `JMP` on the matching side. This keeps it in iABC. So:

| Op | Format | Effect |
|----|--------|--------|
| `TYPEP A C` | iABC | if `RA` does **not** have type-class `C`, skip next. One opcode covers every `*?` predicate; the type enum is extensible without new opcodes. |

### 4.7 Function calls and returns (5)

The calling-convention core. `B` encodes argument count as `B−1` (so `B=0` means "varargs: arguments run from `R[A+1]` to the top of the frame," used by `apply` and multiple-value forwarding). `C` encodes expected result count as `C−1` (`C=0` means "all results, multiple-value context").

| Op | Format | Effect |
|----|--------|--------|
| `CALL A B C` | iABC | Call `RA` with args `R[A+1..A+B-1]`; place `C−1` results starting at `RA`. `B=0`/`C=0` = open-ended (see above). |
| `TAILCALL A B` | iABC | Proper tail call: replace current frame, call `RA` with `R[A+1..A+B-1]`. Never returns to current frame. Mandatory for R7RS. |
| `RETURN A B` | iABC | Return `B−1` values from `R[A..A+B-2]`. `B=0` returns all values from `RA` to top — this is how `values` with a runtime-variable count returns. `B=1` returns zero values. |
| `RETURN1 A` | iABC | Return exactly the single value `RA`. The overwhelmingly common case; avoids the count arithmetic of `RETURN`. |
| `APPLY A B` | iABC | Like `CALL` but the last argument register holds a list to be spread. Implements `apply` without consing a fresh arg vector when avoidable. |

**Multiple values:** `(values 1 2 3)` compiles to placing 1,2,3 in consecutive registers and `RETURN A 4`. `(call-with-values producer consumer)` calls `producer` with `C=0` (accept all results), leaves them in place, then `CALL`s `consumer` with `B=0` (forward all). No dedicated MV machinery — the open-ended counts *are* the mechanism. A single-value continuation receiving multiple values, or vice versa, is detected by the frame's expected-count field.

### 4.8 Closures and variables (7)

| Op | Format | Effect |
|----|--------|--------|
| `CLOSURE A Bx` | iABx | `RA :=` new closure over child prototype `Bx`. Each upvalue's capture source is described by the **child prototype's own descriptor table** (`UpvalDesc`: parent-local register or parent upvalue, in upvalue order) — Lua 5.4's pattern, not 5.1's trailing pseudo-instructions. Captures from the enclosing frame become *open* upvalues. |
| `GETUPVAL A B` | iABC | `RA := U[B]`. Read a captured variable. |
| `SETUPVAL A B` | iABC | `U[B] := RA`. Mutate a captured variable (`set!` on a closed-over binding). Write barrier. |
| `GETGLOBAL A Bx` | iABx | `RA := G[K[Bx]]`. Top-level/library binding read. Signals unbound-variable error if the slot is undefined. |
| `SETGLOBAL A Bx` | iABx | `G[K[Bx]] := RA`. Top-level definition / `set!`. |
| `GETLOCALN A B` | iABC | `RA := R[B]` across a known frame boundary — reserved for the inliner; in RBC-1 emitted only as plain `MOVE`. (Slot reserved, semantics frozen.) |
| `CLOSEUPVALS A` | iABC | Close all open upvalues at register ≥ A without jumping. For block scopes (`let` bodies) that capture and then fall through rather than loop. |

Globals are resolved to integer slots at link time; `K[Bx]` holds the symbol for error reporting and late binding (REPL redefinition — R7RS §5.4 requires redefinition to take retroactive effect, so global access is one indirection through a slot table, never inlined to a direct address).

### 4.9 Vectors, strings, and the runtime bridge (4)

| Op | Format | Effect |
|----|--------|--------|
| `VECREF A B C` | iABC | `RA := (vector-ref RB RC)`. Bounds- and type-checked. Hot enough for an opcode. |
| `VECSET A B C` | iABC | `(vector-set! RA RB RC)`. Bounds-checked, write barrier. |
| `NEWVEC A B C` | iABC | `RA := (make-vector RB RC)` (length, fill). Allocates. |
| `PRIMCALL A B C` | iABC | Call primitive runtime function number `C` (an index into the native-fn table) with `B−1` args from `R[A+1..]`, result in `RA`. **The escape hatch.** Every R7RS procedure without a dedicated opcode — `string-ref`, `assoc`, `sqrt`, `write`, `gcd`, `make-string`, the entire numeric tower's named operations — is a `PRIMCALL`. Adding a library procedure never requires a new opcode. |

`PRIMCALL` vs `CALL`: `CALL` invokes a Scheme closure (pushes a frame, runs bytecode); `PRIMCALL` invokes Rust code with no new bytecode frame. Keeping them distinct lets the VM skip frame setup for primitives and lets primitives be open-coded by a future JIT.

### 4.10 Continuations, control, and concurrency (6)

| Op | Format | Effect |
|----|--------|--------|
| `CAPTURECC A` | iABC | `RA :=` a continuation object snapshotting the current fiber's frame stack, wind stack, and handler stack. Implements `call/cc`. The continuation is an ordinary callable — invoking it is `CALL`. |
| `WINDPUSH A B` | iABC | Push a `dynamic-wind` record: before-thunk `RA`, after-thunk `RB`, onto the fiber wind stack. The compiler emits the before-call, this push, the body, a pop, and the after-call. |
| `WINDPOP` | iABC | Pop the top wind record (normal exit from a `dynamic-wind` body). |
| `HANDLERPUSH A sBx` | iAsBx | Push exception handler `RA`; on `raise`, control transfers to `PC+sBx`. Implements `with-exception-handler`/`guard`. |
| `HANDLERPOP` | iABC | Pop the top handler. |
| `RAISE A B` | iABC | Raise condition `RA`. `B` flag: continuable (`raise-continuable`) vs not (`raise`). Unwinds handler and wind stacks per R7RS §6.11. |

Continuation invocation interacts with the wind stack exactly as R7RS §6.10 specifies: the VM computes the common ancestor of the current and target wind-stack states and runs the appropriate `after`/`before` thunks while unwinding/rewinding. This logic lives in the VM's continuation-restore routine, not in bytecode.

The concurrency opcodes from the RUVM spec (`SPAWN`, `YIELD`, `RESUME`, `CHANMAKE`, `CHANSEND`, `CHANRECV`) are **deferred to RBC-2**. They depend on the fiber scheduler, which postdates a working sequential VM. Reserving their opcode numbers now (`0x40`–`0x45`) prevents renumbering churn later.

---

## 5. Opcode Number Assignment

This table is **frozen** — it is the discriminant list of the `Op` enum in `src/bytecode/op.rs`, finalized when the decoder was written. Grouped so related ops share cache lines in the match arms; the gaps (`0x0C`–`0x0F`, `0x1E`–`0x1F`, `0x2A`–`0x2F`, `0x3E`–`0x3F`) are room for each group to grow without renumbering its neighbours.

```
0x00 MOVE        0x10 NUMEQ       0x20 CADR        0x30 GETGLOBAL
0x01 LOADK       0x11 NUMLT       0x21 TYPEP       0x31 SETGLOBAL
0x02 LOADKX      0x12 NUMLE       0x22 CALL        0x32 GETLOCALN
0x03 LOADIMM     0x13 EQ          0x23 TAILCALL    0x33 CLOSEUPVALS
0x04 LOADI       0x14 EQV         0x24 RETURN      0x34 VECREF
0x05 ADD         0x15 TEST        0x25 RETURN1     0x35 VECSET
0x06 SUB         0x16 JMP         0x26 APPLY       0x36 NEWVEC
0x07 MUL         0x17 EXTRAARG    0x27 CLOSURE     0x37 PRIMCALL
0x08 DIV         0x18 JMPIDX      0x28 GETUPVAL    0x38 CAPTURECC
0x09 QUOT        0x19 CONS        0x29 SETUPVAL    0x39 WINDPUSH
0x0A NEG         0x1A CAR         0x2A (reserved)  0x3A WINDPOP
0x0B ADDI        0x1B CDR         ...              0x3B HANDLERPUSH
0x0C (reserved)  0x1C SETCAR      0x2F (reserved)  0x3C HANDLERPOP
...              0x1D SETCDR                       0x3D RAISE
0x0F (reserved)  0x1E (reserved)                   0x3E (reserved)
                 0x1F (reserved)                   0x3F (reserved)

0x40..0x45 reserved for RBC-2 concurrency.
```

Renumbering, should it ever be needed, is deliberately cheap: nothing outside `src/bytecode/op.rs` matches on raw bytes, and every test asserts on disassembly text — never on instruction words.

---

## 6. Worked Compilations

These listings are the disassembler's own output for hand-assembled prototypes, verified by the load-time verifier and frozen as `insta` snapshots in `tests/disasm.rs`. They supersede this spec's earlier schematic sketches, which took liberties (constants in register operands, approximate offsets) that a real encoding cannot.

### 6.1 Tail-recursive factorial

```scheme
(define (fact n acc)
  (if (= n 0) acc (fact (- n 1) (* n acc))))
```

```
; proto fact: nparams=2 has_rest=false window=5 consts=2 upvals=0 protos=0
0000  LOADK       r2, k0                ; 0
0001  NUMEQ       r0, r2, 0             ; skip if (= r0 r2)
0002  JMP         +1                    ; -> 0004
0003  RETURN1     r1
0004  GETGLOBAL   r2, k1                ; fact
0005  ADDI        r3, r0, -1
0006  MUL         r4, r0, r1
0007  TAILCALL    r2, 3                 ; 2 args
```

Notes on the derivation: `NUMEQ` compares two registers, so the constant `0` is loaded first; its `k` flag is `0`, so the `JMP` to the recursive arm executes when `(= n 0)` is *false* and is skipped — falling into `RETURN1 acc` — when it is true. `(- n 1)` is `ADDI -1`. The callee and both arguments are laid out contiguously in `r2..r4`, so `TAILCALL` reuses the frame with no shuffling: constant stack space, mandatory, not optional.

### 6.2 A closure capturing a variable

```scheme
(define (make-counter)
  (let ((n 0))
    (lambda () (set! n (+ n 1)) n)))
```

```
; proto make-counter: nparams=0 has_rest=false window=2 consts=0 upvals=0 protos=1
0000  LOADI       r0, 0
0001  CLOSURE     r1, p0
0002  CLOSEUPVALS r0
0003  RETURN1     r1

; proto make-counter.p0: nparams=0 has_rest=false window=1 consts=0 upvals=1 protos=0
;   u0 <- parent local r0
0000  GETUPVAL    r0, u0
0001  ADDI        r0, r0, 1
0002  SETUPVAL    r0, u0                ; u0 := r0
0003  RETURN1     r0
```

The capture is described by the child prototype's descriptor table (`u0 <- parent local r0`), read by `CLOSURE` — there are no trailing pseudo-instructions. The upvalue starts *open* (pointing at `make-counter`'s `r0`); `CLOSEUPVALS 0` moves it into a heap cell as the frame exits, so the counter persists and two counters get independent cells.

### 6.3 call/cc escape

```scheme
(+ 1 (call/cc (lambda (k) (k 10))))
```

```
; proto main: nparams=0 has_rest=false window=4 consts=0 upvals=0 protos=1
0000  LOADI       r1, 1
0001  CLOSURE     r2, p0
0002  CAPTURECC   r3
0003  CALL        r2, 2, 2              ; 1 arg, 1 result
0004  ADD         r0, r1, r2
0005  RETURN1     r0

; proto main.p0: nparams=1 has_rest=false window=2 consts=0 upvals=0 protos=0
0000  LOADI       r1, 10
0001  TAILCALL    r0, 2                 ; 1 arg
```

`CAPTURECC` writes the continuation directly into `r3` — the call's argument slot — so no `MOVE` is needed. Inside the lambda, `(k 10)` is in tail position and the continuation is an ordinary callable, so invoking it is a `TAILCALL`; the VM recognizes the callee as a continuation and restores the snapshot, delivering 10 to the `ADD`. Result: 11.

---

## 7. Open Questions for Implementation

1. **`CADR` path encoding** — confirm the C-byte layout (length nibble + direction bits) handles the full `caar`…`cddddr` set (max depth 4 → fits in 4 bits + 4-bit length). It does, but lock the bit layout before the compiler emits it.
2. **`TYPEP` type enum** — finalize the integer assignments and whether disjoint-type fast checks (pair/null/fixnum) get a branch ahead of the general table lookup.
3. **Global slot table growth** — REPL redefinition needs slots to be append-only and stable. Decide whether library boundaries get separate slot namespaces.
4. **`DIV` policy** — confirm with the numeric-tower design that the "divides evenly" fast path is worth the branch, or whether `DIV` should always go to the runtime and `QUOT` carries the fast integer path alone.
5. **Line-info table format** — provisionally answered in M2: `Proto.spans` is a per-instruction table of source byte spans (empty when debug info is stripped), because the reader already produces byte spans and miette consumes them directly. A compressed encoding can replace the representation behind that field if it ever matters; revisit before the compiler emits it at scale.

