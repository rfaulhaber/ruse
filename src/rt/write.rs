//! The printer: R7RS `display` and `write` rendering, minus what later milestones own.
//!
//! `Display` is for humans (strings and characters render as themselves); `Write` is for
//! the reader (strings quoted and escaped, characters as `#\` literals). The datum-label
//! cycle machinery (`write-shared`, and termination on mutated circular data) is M9's;
//! until then a cyclic structure loops, which is the same trade `equal?` makes.
//!
//! Traversal is worklist-driven like every other structural walk in the crate — a
//! 200k-cell list must print, not overflow the Rust stack.

use crate::gc::Heap;
use crate::value::Value;
use crate::value::layout::HeapTag;
use crate::value::object::{Bignum, Bytevector, Closure, NativeProc, Pair, Str, Symbol, Vector};

/// Which audience the text is for.
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum Style {
    /// Human-readable: `(display "hi")` prints `hi`.
    Display,
    /// Reader-readable: `(write "hi")` prints `"hi"`.
    Write,
}

/// One pending emission. Structural punctuation that must appear *after* a value's
/// children is queued as text, which is what keeps the walk iterative.
enum Task {
    Value(Value),
    /// The rest of a list: emits ` elem`… then `)`, or ` . tail)` for improper lists.
    Cdr(Value),
    Text(&'static str),
}

/// Render `v` as text.
pub fn value_to_string(heap: &Heap, v: Value, style: Style) -> String {
    let mut out = String::new();
    let mut work = vec![Task::Value(v)];

    while let Some(task) = work.pop() {
        match task {
            Task::Text(s) => out.push_str(s),
            Task::Value(v) => {
                if let Some(p) = heap.get::<Pair>(v) {
                    out.push('(');
                    work.push(Task::Cdr(p.cdr));
                    work.push(Task::Value(p.car));
                } else if let Some(vec) = heap.get::<Vector>(v) {
                    out.push_str("#(");
                    work.push(Task::Text(")"));
                    for (i, &elem) in vec.elems.iter().enumerate().rev() {
                        work.push(Task::Value(elem));
                        if i > 0 {
                            work.push(Task::Text(" "));
                        }
                    }
                } else {
                    atom(heap, v, style, &mut out);
                }
            }
            Task::Cdr(v) => {
                if let Some(p) = heap.get::<Pair>(v) {
                    out.push(' ');
                    work.push(Task::Cdr(p.cdr));
                    work.push(Task::Value(p.car));
                } else if v.is_null() {
                    out.push(')');
                } else {
                    out.push_str(" . ");
                    work.push(Task::Text(")"));
                    work.push(Task::Value(v));
                }
            }
        }
    }
    out
}

/// Everything that has no children.
fn atom(heap: &Heap, v: Value, style: Style, out: &mut String) {
    if let Some(n) = v.as_fixnum() {
        out.push_str(&n.to_string());
        return;
    }
    if let Some(x) = v.as_flonum() {
        out.push_str(&flonum_text(x));
        return;
    }
    if let Some(c) = v.as_char() {
        match style {
            Style::Display => out.push(c),
            Style::Write => out.push_str(&char_literal(c)),
        }
        return;
    }
    if let Some(b) = v.as_boolean() {
        out.push_str(if b { "#t" } else { "#f" });
        return;
    }
    if v.is_null() {
        out.push_str("()");
        return;
    }
    if v.is_eof() {
        out.push_str("#<eof>");
        return;
    }
    if v.is_unspecified() {
        out.push_str("#<unspecified>");
        return;
    }
    if v.is_undefined() {
        out.push_str("#<undefined>");
        return;
    }
    match heap.tag_of(v) {
        Some(HeapTag::Str) => {
            let Some(s) = heap.get::<Str>(v) else { return };
            match style {
                Style::Display => out.push_str(&s.chars),
                Style::Write => quote_string(&s.chars, out),
            }
        }
        Some(HeapTag::Symbol) => {
            if let Some(s) = heap.get::<Symbol>(v) {
                out.push_str(&s.name);
            }
        }
        Some(HeapTag::Bignum) => {
            if let Some(b) = heap.get::<Bignum>(v) {
                out.push_str(&b.value.to_string());
            }
        }
        Some(HeapTag::Bytevector) => {
            let Some(b) = heap.get::<Bytevector>(v) else {
                return;
            };
            out.push_str("#u8(");
            for (i, byte) in b.bytes.iter().enumerate() {
                if i > 0 {
                    out.push(' ');
                }
                out.push_str(&byte.to_string());
            }
            out.push(')');
        }
        Some(HeapTag::Closure) => match heap.get::<Closure>(v).and_then(|c| c.proto.name.clone()) {
            Some(name) => {
                out.push_str("#<procedure ");
                out.push_str(&name);
                out.push('>');
            }
            None => out.push_str("#<procedure>"),
        },
        Some(HeapTag::NativeProc) => match heap.get::<NativeProc>(v) {
            Some(p) => {
                out.push_str("#<procedure ");
                out.push_str(&p.name);
                out.push('>');
            }
            None => out.push_str("#<procedure>"),
        },
        Some(HeapTag::UpvalueCell) => out.push_str("#<upvalue>"),
        Some(HeapTag::Record) => out.push_str("#<record>"),
        Some(HeapTag::RecordType) => out.push_str("#<record-type>"),
        // Pairs and vectors were handled structurally above.
        Some(HeapTag::Pair | HeapTag::Vector) | None => out.push_str("#<invalid>"),
    }
}

/// R7RS external representation of an inexact real: always distinguishable from an exact
/// integer, so `1.0` never prints as `1`.
fn flonum_text(x: f64) -> String {
    if x.is_nan() {
        return "+nan.0".to_string();
    }
    if x.is_infinite() {
        return if x > 0.0 { "+inf.0" } else { "-inf.0" }.to_string();
    }
    // Rust's `Display` never uses scientific notation, so 1e300 would print as three
    // hundred digits; switch to it outside a readable magnitude band.
    let s = if x != 0.0 && (x.abs() < 1e-10 || x.abs() >= 1e21) {
        format!("{x:e}")
    } else {
        format!("{x}")
    };
    if s.contains(['.', 'e', 'E']) {
        s
    } else {
        format!("{s}.0")
    }
}

fn char_literal(c: char) -> String {
    // The R7RS §6.6 character names.
    let name = match c {
        '\u{7}' => Some("alarm"),
        '\u{8}' => Some("backspace"),
        '\u{7f}' => Some("delete"),
        '\u{1b}' => Some("escape"),
        '\n' => Some("newline"),
        '\0' => Some("null"),
        '\r' => Some("return"),
        ' ' => Some("space"),
        '\t' => Some("tab"),
        _ => None,
    };
    match name {
        Some(name) => format!("#\\{name}"),
        None if c.is_control() => format!("#\\x{:x}", c as u32),
        None => format!("#\\{c}"),
    }
}

fn quote_string(s: &str, out: &mut String) {
    out.push('"');
    for c in s.chars() {
        match c {
            '"' => out.push_str("\\\""),
            '\\' => out.push_str("\\\\"),
            '\n' => out.push_str("\\n"),
            '\t' => out.push_str("\\t"),
            '\r' => out.push_str("\\r"),
            c if c.is_control() => {
                out.push_str(&format!("\\x{:x};", c as u32));
            }
            c => out.push(c),
        }
    }
    out.push('"');
}

#[cfg(test)]
mod tests {
    use super::*;

    fn fix(n: i64) -> Value {
        Value::fixnum(n).unwrap()
    }

    fn wr(heap: &Heap, v: Value) -> String {
        value_to_string(heap, v, Style::Write)
    }

    fn disp(heap: &Heap, v: Value) -> String {
        value_to_string(heap, v, Style::Display)
    }

    #[test]
    fn numbers_print_by_representation() {
        let mut heap = Heap::new();
        assert_eq!(wr(&heap, fix(-42)), "-42");
        assert_eq!(wr(&heap, Value::flonum(1.0)), "1.0");
        assert_eq!(wr(&heap, Value::flonum(-0.5)), "-0.5");
        assert_eq!(wr(&heap, Value::flonum(-0.0)), "-0.0");
        assert_eq!(wr(&heap, Value::flonum(1e300)), "1e300");
        assert_eq!(wr(&heap, Value::flonum(f64::NAN)), "+nan.0");
        assert_eq!(wr(&heap, Value::flonum(f64::INFINITY)), "+inf.0");
        assert_eq!(wr(&heap, Value::flonum(f64::NEG_INFINITY)), "-inf.0");
        let big = heap.integer(crate::value::FIXNUM_MAX + 1);
        assert_eq!(wr(&heap, big), "140737488355328");
    }

    #[test]
    fn style_separates_the_two_audiences() {
        let mut heap = Heap::new();
        let s = heap.string("say \"hi\"\n");
        assert_eq!(disp(&heap, s), "say \"hi\"\n");
        assert_eq!(wr(&heap, s), "\"say \\\"hi\\\"\\n\"");

        assert_eq!(disp(&heap, Value::char('q')), "q");
        assert_eq!(wr(&heap, Value::char('q')), "#\\q");
        assert_eq!(wr(&heap, Value::char(' ')), "#\\space");
        assert_eq!(wr(&heap, Value::char('\n')), "#\\newline");
        assert_eq!(wr(&heap, Value::char('\u{1}')), "#\\x1");
    }

    #[test]
    fn lists_proper_improper_and_nested() {
        let mut heap = Heap::new();
        let nil = Value::NIL;
        assert_eq!(wr(&heap, nil), "()");

        let l = {
            let tail = heap.cons(fix(3), Value::NIL);
            let mid = heap.cons(fix(2), tail);
            heap.cons(fix(1), mid)
        };
        assert_eq!(wr(&heap, l), "(1 2 3)");

        let dotted = heap.cons(fix(1), fix(2));
        assert_eq!(wr(&heap, dotted), "(1 . 2)");

        let nested = {
            let inner = heap.cons(fix(2), Value::NIL);
            let inner = heap.cons(inner, Value::NIL);
            heap.cons(fix(1), inner)
        };
        assert_eq!(wr(&heap, nested), "(1 (2))");
    }

    #[test]
    fn vectors_bytevectors_and_the_rest() {
        let mut heap = Heap::new();
        let s = heap.string("x");
        let v = heap.vector(vec![fix(1), s, Value::TRUE]);
        assert_eq!(wr(&heap, v), "#(1 \"x\" #t)");
        assert_eq!(disp(&heap, v), "#(1 x #t)");
        let empty = heap.vector(vec![]);
        assert_eq!(wr(&heap, empty), "#()");

        let bv = heap.bytevector(vec![0, 255]);
        assert_eq!(wr(&heap, bv), "#u8(0 255)");

        let sym = heap.symbol("call/cc");
        assert_eq!(wr(&heap, sym), "call/cc");
        assert_eq!(wr(&heap, Value::TRUE), "#t");
        assert_eq!(wr(&heap, Value::EOF), "#<eof>");

        let native = heap.native_proc("car", 0);
        assert_eq!(wr(&heap, native), "#<procedure car>");
    }

    /// The reason the walk is iterative.
    #[test]
    fn a_very_long_list_prints_without_recursing() {
        const CELLS: usize = if cfg!(miri) { 2_000 } else { 200_000 };
        let mut heap = Heap::new();
        let mut l = Value::NIL;
        for _ in 0..CELLS {
            l = heap.cons(fix(9), l);
        }
        let text = disp(&heap, l);
        assert_eq!(text.len(), 2 * CELLS + 1); // "9 " per cell, minus one space, plus "()".
        assert!(text.starts_with("(9 9"));
        assert!(text.ends_with("9)"));
    }
}
