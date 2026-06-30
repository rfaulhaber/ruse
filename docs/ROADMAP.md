# Ruse Implementation Roadmap

Taking **ruse** from a frontend-only reader to a working R7RS Scheme on the RBC-1
register bytecode VM (`ruse-bytecode-spec.md`).

> Status as of this writing: lexer + two parsers + span-carrying `Expr` AST + a
> parse-only REPL. No evaluator, compiler, `Value` type, GC, numeric tower, macro
> expander, or standard library. `src/vm.rs` is a placeholder. The R7RS conformance
> suite (`tests/r7rs_suite/r7rs.scm`, ~73 KB) is unwired and, as written, all-or-nothing.

## Frozen decisions

These were ratified by the project owner and supersede the spec where they conflict.

1. **Opcode set is frozen at 50 real opcodes**, `0x40`–`0x45` reserved for RBC-2
   concurrency. The spec's "56 opcodes / nine groups" prose is wrong; §4 enumerates
   ten groups summing to 50. Tests key on **disassembly text, not raw bytes**, so the
   byte table can be renumbered cheaply if needed.
2. **R7RS semantics are followed strictly**, even where it costs performance:
   - `DIV` of exact operands yields an exact rational (`(/ 1 3)` ⇒ `1/3`); the fixnum
     fast path fires only when the division is exact (R7RS §6.2.6, spec §1.2).
   - `with-exception-handler` installs a **non-transferring, in-place** handler so
     `raise-continuable` can return a value. Only `guard` transfers control. The spec's
     §4.10 `HANDLERPUSH … PC+sBx` control-transfer wording is **guard-only**.
   - Inexact divide-by-zero `(/ 1.0 0.0)` yields `+inf.0` (IEEE), not a signal.

## Architecture decisions

| # | Decision | Rationale (vs. rejected) |
|---|----------|--------------------------|
| A | `Value` is `repr(transparent) struct Value(u64)`, NaN-boxed: immediate flonums, 48-bit fixnums, chars, 6 singletons, one heap-pointer tag (concrete type in the object header). Hardware NaNs canonicalized to a **fixed non-signature** pattern so `+nan.0` stays an immediate flonum and is never misread as a pointer. | Spec §2 mandates NaN-boxing. `cfg`-gated tagged-enum fallback for debugging and LA57 (57-bit virtual address) platforms. *vs. 61-bit tagged int, plain tagged enum.* |
| B | GC is **precise, non-moving, stop-the-world tri-color mark-sweep**, `Drop`-on-sweep. Write barriers emitted as no-ops from day one at `SETCAR`/`SETCDR`/`VECSET`/`SETUPVAL`/`SETGLOBAL`. | Non-moving ⇒ boxed pointers in register windows never need fixups; `Drop` finalizes `num-bigint`/`String` storage on the Rust heap. *vs. copying/generational (premature fixups), refcount (unsound for cycles/continuations).* |
| C | Compiler is direct syntax-directed codegen (destination-register + tail-flag descent) over a typed **Core IR**, with a Lua-5 `FuncState` register allocator (free-reg high-water, active-locals stack, constant interner, jump-patch list, window ≤ 250). No CPS, no ANF. | A contiguous-window register VM pairs naturally with dest+tail descent; ANF/CPS don't model the contiguous call window. The Core IR separates special-form parsing, identifier resolution, and load-target selection that the string-keyed `Expr` conflates. |
| D | Numeric tower on **`num-bigint`/`num-rational`/`num-integer`/`num-traits`**. 48-bit fixnum with centralized promote-to-bignum / demote-to-fixnum. Complex is re+im over reals (exact complex representable). | Pure Rust, Nix-friendly, no GMP/GPL/C. *vs. `rug`/GMP, machine-float complex.* |
| E | Hygiene via Flatt **scope-sets**; a `Syntax` type (scope-extended symbol preserving `Span`) designed *before* the expander. An unhygienic gensym-rename prototype lands first to unblock the derived-form prelude. | Scope-sets compose across internal-define/`letrec*` where the suite's adversarial hygiene tests live. Hygiene can't be bolted on later. *vs. KFFD mark-and-rename.* |
| F | One `Continuation` heap object, callable. **Escape-only capture first**, then upgrade the same object to multi-shot via whole-stack copy. Capture closes open upvalues into heap cells. | Contiguous register array forecloses O(1) capture; copying gives full re-entrancy (suite's re-entrant `dynamic-wind` generator needs it). *vs. one-shot/segmented first (forces a rewrite).* |
| G | Stdlib in 3 layers: opcode-backed prims, a `PRIMCALL` native-fn table returning `Outcome` (one/many/error), and a baked-in `prelude.scm` (`include_str!`, compiled at boot) for higher-order procedures. Every prim is **also** a global-slot procedure; `PRIMCALL` is emitted only as an optimization for statically-known un-shadowed globals. | Fixes the 256-`PRIMCALL` ceiling and preserves §5.4 redefinability. (Re-entrant natives are *possible* via a host trampoline, but prelude-resident HOFs keep the Rust surface small.) |
| H | `read` produces a runtime **`Value` datum graph** (cyclic-capable), not the tree `Expr`. The compiler/expander consume a `Syntax` wrapper over data. | A tree AST can't represent self-referential `#0=(1 . #0#)`; datum-label constants land in constant pools as real cyclic `Value`s. *vs. keeping `Expr` + two-pass label patching.* |

## Milestones

Strictly ordered. `Value`+GC underpin everything; the VM and compiler co-evolve to a
"walking skeleton" (first end-to-end eval, proper tail calls baked in) **before** breadth.
Each milestone ends in a demoable, testable artifact; slices of `r7rs.scm` are wired in as
early as feasible. Effort is relative (S/M/L/XL).

### M0 — Repo hygiene, lint scoping, spec freeze  · S
Green and decision-ready. **Exit:** `cargo clippy --all-targets -- -D warnings` passes;
one parser engine; `cargo nextest run` green; frozen decisions recorded.
- Scope clippy: `#![cfg_attr(test, allow(...))]` for the inline-test `unwrap`/`panic`
  lints; move REPL/CLI into the bin target so the library stays print-free and the
  `print_*` denies meaningfully guard the library; fix the one real production `unwrap`
  at `src/lexer.rs:174`.
- Collapse the duplicate `Parser`/`StreamingParser` into a single reader (the real
  streaming `read`-over-ports reader returns in M6 producing `Value`s).
- `src/vm.rs` stub: 256-register comment → the spec's 250-register window (R0–R249).
- Quick lexer fixes that are preconditions later: route `+`/`-`+digit through the number
  path (signed literals stop mis-lexing as symbols); tokenize `...` as a single
  identifier (precondition for `syntax-rules`).
- Modernize CI: `cargo nextest run`; drop/allow-fail the beta/nightly matrix (toolchain
  is pinned); `Swatinem/rust-cache`; harden clippy to `--all-targets -- -D warnings`.

### M1 — NaN-boxed `Value` + minimal precise GC  · L · deps: M0
**Exit:** construct pairs/vectors/strings/symbols/bignums, trigger a collection, prove
live survive and dead `Drop`; `eq?` identity on interned symbols.
- `Value(u64)` with immediate constructors + NaN canonicalization + round-trip tests.
- `make_integer` with centralized fixnum↔bignum promote/demote.
- `repr(C)` `GcHeader` (tag/color/flags); `HeapTag`: Pair, Str, Symbol, Vector,
  Bytevector, Closure, UpvalueCell, Bignum, **Record, RecordType** (§5.5).
- Global symbol interner (`eq?` = pointer identity).
- Precise non-moving tri-color mark-sweep with `Drop`-on-sweep; root API.
- **Roots must include the symbol interner table and a handle/pin protocol for native-fn
  (`PRIMCALL`) temporaries** — both are easy-to-miss precise-GC holes.
- Write-barrier API `wb(container, new)` as a no-op, ready for the mutating opcodes.

### M2 — Instruction encoding, decoder, `Op` enum, verifier, disassembler  · M · deps: M1
**Exit:** hand-assembled `Proto`s round-trip encode→decode→disassemble as stable mnemonic
text; verifier rejects bad operands; the disassembler reproduces spec §6.1 factorial.
- `Insn(u32)` (spec §3.1 accessors) + symmetric encoder constructors.
- `Op` enum (`repr(u8)`), **50 opcodes**, `0x40`–`0x45` reserved.
- Encode-side `Proto` (code, const pool as `Value`s, upvals, child protos, arity, max
  window, line table); `UpvalDesc` = ParentLocal | ParentUpval.
- Load-time verifier (operand ranges, window ≤ 250, in-bounds indices).
- `src/disasm.rs`; `insta` snapshots of **disassembly text, never raw bytes**.
- Note: spec §6 worked examples are schematic/inconsistent — derive correct encodings.

### M3 — Walking skeleton: VM dispatch loop + core codegen → first eval  · XL · deps: M1, M2
**Exit:** tail-recursive `fact` runs to the right result in **constant** frame depth;
simple arithmetic/list/`define` programs print; first non-zero `r7rs.scm` slice passes.
- VM: flat per-fiber register file (`Vec<Value>`), `Frame` stack, `match`-on-`u8`
  dispatch, **usize-index addressing** (never hold a `&mut` slice across a nested call).
- Opcode subset: MOVE, LOADI/LOADK/LOADIMM, GET/SETGLOBAL (append-only slots, unbound
  detection), ADD/SUB/MUL fixnum-fast, NUMEQ/NUMLT/NUMLE + EQ/EQV (skip-next), TEST, JMP,
  CALL/TAILCALL/RETURN/RETURN1, CLOSURE (no captures yet), PRIMCALL.
- Every error path is a typed `VmError` `Result` (no `unwrap`/`unreachable`, per the lint wall).
- Compiler: Core IR, special-form classification (define/lambda/if/quote/set!/begin/let),
  lexical addressing, Lua-`FuncState` allocation, tail-position propagation, fixed-arity
  lambda→child `Proto`+CLOSURE, `ADDI` peephole.
- Minimal `display`/`write`/`newline` + `cons`/`car`/`cdr`/`eq?` so output is observable;
  **a non-cyclic `equal?`** (the `(test …)` shim compares with it).
- `eval_str` wiring read→compile→run→write; `main.rs` file path replaces the TODO.
- The `(chibi test)` shim + an error-tolerant incremental conformance driver (skip on
  unbound-identifier) scoring a passing-form tally.

### M4 — Closures/upvalues + full binding & derived forms  · L · deps: M3
**Exit:** spec §6.2 `make-counter` yields two **independent** counters; `set!` on a
captured var mutates the shared binding; `let`-family/`cond`/`case`/`and`/`or` slice passes.
- CLOSURE + upvalue descriptors; Lua-style open→closed upvalue list; GET/SETUPVAL through
  cells with the SETUPVAL barrier; CLOSEUPVALS / `JMP A>0` for loop-exit closing.
- Variadic/rest lambdas (`has_rest`); internal defines as `letrec*` with the `undefined`
  black-hole and forward-reference errors.
- Derived forms (built-in core syntax until M8): `and`/`or`/`when`/`unless`/`cond`
  (incl. `=>`/`else`), `case` (EQV chains; **incl. `=>` clauses**; JMPIDX deferred),
  `let*`/`letrec`/`letrec*`/named `let`/`do`, **`let-values`/`let*-values`** (§4.2.2,
  needed by the numeric section).
- `quasiquote`/`unquote`/`unquote-splicing` expansion with depth tracking (ensure
  `append`/`list->vector` are available when this codegen lands).

### M5 — Numeric tower + §7.1.1 number reader  · XL · deps: M1, **M3**
**Exit (narrowed):** bignums, reduced rationals, `#e`/`#i` prefixes, exact complex,
inf/NaN read+compute+print; the signed-literal lex bug is fixed; non-gating differential
vs. chibi on a proptest mix. *`exact-integer-sqrt`/`floor/`/`truncate/` (multiple values)
defer to after M7.*
- Tower types (bignum, gcd-normalized rational, flonum incl. ±inf/NaN, exact+inexact
  complex); generic dispatch with exact/inexact contagion; `DIV` exact-rational trap;
  `QUOT` integer fast path; NaN-aware comparisons.
- Predicates + exact/inexact conversion; `number->string`/`string->number` with radix.
- Number sub-reader (full grammar: radix/exactness prefixes, rationals, decimals, inf/NaN,
  `±i`); fix the `+`/`.`-prefixed dispatch and the dead minus-sign code (`lexer.rs:280`).

### M6 — Reader datum breadth + runtime `read`  · L · deps: M1, M5, **M3**
**Exit:** vectors, bytevectors, nested `#|…|#`, `#;`, full char/string escapes, `|…|`
identifiers, and a cyclic `#0=…` datum all read into runtime `Value`s; reading `'x` yields
the list `(quote x)`; reader-heavy suite sections run.
- `Vector`/`Bytevector` data (bytes validated 0–255) + openers.
- Nested block comments; `#;` (read-and-discard one datum); full char names + `#\xHH`;
  string escapes (`\xHH;`, `\a`, `\b`, line-continuation); pipe identifiers; `#true`/`#false`;
  ellipsis + peculiar identifiers; token-delimiter enforcement.
- Datum labels (shared/cyclic `Value` graphs, placeholder patching); `#!fold-case` state.
- Runtime `read` (string-source first; `Port` type lands in M9); quote sugar desugared.

### M7 — First-class control  · XL · deps: M3, M4
call/cc, dynamic-wind, exceptions, multiple values, parameters — the highest-risk runtime.
**Exit:** escape continuations; re-entrant `dynamic-wind` generator ordering;
`call-with-values`; `raise-continuable` returns to the raise point; `guard` (incl. `=>`,
nested re-raise); `parameterize` under call/cc.
- Multiple values via open-ended CALL/RETURN counts (accept-all / forward-all); fiber
  value-count plumbed through returns and continuation invocation.
- CAPTURECC: escape-only first, then multi-shot full-copy; capture closes open upvalues.
- Wind stack + common-ancestor unwind/rewind (re-entrant VM calls).
- Handler stack; `with-exception-handler` **non-transferring** (so `raise-continuable`
  returns); `guard` lowered to call/cc + with-exception-handler; condition/error-object type.
- `make-parameter`/`parameterize` on the wind stack (captured continuations restore params).
- Compiler suppresses TAILCALL for `dynamic-wind`/`with-exception-handler` body thunks
  (those positions are **not** tail — getting this wrong skips after-thunks/handlers).

### M8 — Hygienic `syntax-rules` + library system  · XL · deps: M3, M4, **M6**
**Exit:** suite §4.3 hygiene block passes (introduced-binding non-capture, aux-keyword
shadowing, custom/escaped ellipsis, nested/improper patterns, macro-defining + expand-to-
define); the top-of-file `import`s resolve.
- `Syntax` (scope-sets) + fresh-scope minting + span-preserving expansion.
- `syntax-rules` pattern compiler + match-tree + template instantiation; `define-syntax`/
  `let-syntax`/`letrec-syntax`.
- Fixpoint expander over a compile-time env; body expander classifying interleaved
  define/define-syntax/define-values/expr as `letrec*`.
- **`define-record-type`** (§5.5): generated constructor/predicate/accessor/mutator.
- Re-express M4 derived forms as a bootstrap `syntax-rules` prelude (keep `let`/`letrec`
  as compiler builtins for diagnostics).
- Library registry; import-set modifiers (`only`/`except`/`prefix`/`rename`) as slot
  **aliasing** over one append-only global slot vector; `cond-expand`; `include`/`include-ci`.

### M9 — Stdlib breadth, ports/IO, full conformance  · XL · deps: M5, M6, M7, M8
**Exit:** `r7rs.scm` loads end-to-end through the incremental driver with a high, tracked
passing-form count; score published per CI run.
- `Port` heap type (string/bytevector/file in+out) + I/O surface; runtime `read`; full
  `write`/`display` with datum-label cycle detection (`write-shared`/`write-simple`).
- Cycle-safe `equal?`; boolean/symbol/char/string/vector/bytevector surfaces; **Unicode
  case-mapping data** for `char`/`string` case ops, `char-ci=?`/`string-ci=?`, `digit-value`.
- `prelude.scm` HOFs (`map`/`for-each`/`vector-map`/`string-map`/fold/`assoc`/`member`
  families/`force`); `delay`/`delay-force`/`make-promise` + `Promise` type; `case-lambda`;
  multiple-value numeric ops (`floor/`, `truncate/`, `exact-integer-sqrt`) over single-value prims.
- `(scheme inexact)`/`(scheme complex)` math; `(scheme process-context/time/file/eval/repl/
  load)` behind cargo features. **Register stub/feature-gated libraries so the suite's top
  `import` resolves** even when procedures are unimplemented.
- **Literal-constant immutability** (§4.1.2/§6.7): `set-car!`/`string-set!` on a pooled
  quoted/literal must error or copy.
- Conformance CI: gating `nextest`; tracked non-gating `r7rs.scm` score; non-gating
  differential vs. chibi-scheme.

## Open spec/design questions still to pin (before the relevant code freezes)

- **`CADR` C-byte bit layout** (spec open-Q 1) and the **`TYPEP` type-selector enum**
  (open-Q 2) — freeze jointly between VM and compiler. Until then emit plain `CAR`/`CDR`
  chains and EQV chains for `case`.
- **Global slot namespace across libraries** (open-Q 3): one append-only slot vector with
  import-as-aliasing vs. per-library namespaces. Constrains GETGLOBAL resolution and REPL
  retroactive redefinition (§5.4). Slot key must be decoupled from the constant-pool
  source-name string used for diagnostics.
- **`PRIMCALL` conventions** beyond 256 (globals-as-procedures fallback) and the variadic /
  multiple-value `PRIMCALL` calling conventions the spec leaves unspecified.
- **Non-R7RS extensions**: keep `[ ]` bracket lists and the `#!eof`/`#!default` singletons,
  or drop them? Commit to exact complex (R7RS permits omitting it, but the suite tests it)?
