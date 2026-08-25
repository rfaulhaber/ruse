//! The disassembler: one [`Proto`] tree in, stable mnemonic text out.
//!
//! This text is the project's test surface for everything bytecode-shaped. Tests assert
//! against it — never against raw instruction words — so the opcode byte table stays cheap
//! to renumber (a frozen decision; see `docs/project_plan.org`). That gives the format two
//! obligations: it must be *stable*, because `insta` snapshots freeze it, and it must be
//! *complete enough* to distinguish any two encodings a correct compiler could emit.
//!
//! The listing never prints a heap address, because addresses change run to run. Heap
//! constants render through their content (symbol names, string bodies, bignum digits) or
//! as an opaque `#<type>`.
//!
//! The disassembler is a debugging tool, so it does not require a verified prototype: an
//! unknown opcode byte prints as `INVALID` and out-of-range indices are annotated rather
//! than panicking. It does assume roughly-canonical encodings — fields an opcode does not
//! use are not printed, so run [`verify`](crate::bytecode::verify()) when the question is
//! "is this well-formed" rather than "what is this".

use crate::bytecode::insn::Insn;
use crate::bytecode::op::Op;
use crate::bytecode::proto::{Proto, UpvalDesc};
use crate::bytecode::verify::{child_label, root_label};
use crate::gc::Heap;
use crate::value::Value;
use crate::value::layout::{
    HeapTag, SINGLETON_EOF, SINGLETON_FALSE, SINGLETON_NULL, SINGLETON_TRUE, SINGLETON_UNDEFINED,
    SINGLETON_UNSPECIFIED,
};
use crate::value::object::{Bignum, Str, Symbol};

/// The column where `; comments` begin, when the operands leave room.
const COMMENT_COL: usize = 40;

/// Disassemble `root` and every child prototype, depth first.
///
/// `heap` is where the constant pools' values live; rendering them carries
/// [`Heap::get`]'s liveness obligation, which at load time — the only time a prototype is
/// disassembled — is exactly the obligation the loader already has.
///
/// ```
/// use ruse::bytecode::{Insn, Op, Proto};
/// use ruse::{Heap, disasm};
///
/// let heap = Heap::new();
/// let proto = Proto {
///     name: Some("answer".into()),
///     code: vec![Insn::iasbx(Op::LoadI, 0, 42), Insn::iabc(Op::Return1, 0, 0, 0)],
///     max_window: 1,
///     ..Proto::default()
/// };
/// assert_eq!(
///     disasm::disassemble(&heap, &proto),
///     "\
/// ; proto answer: nparams=0 has_rest=false window=1 consts=0 upvals=0 protos=0
/// 0000  LOADI       r0, 42
/// 0001  RETURN1     r0
/// "
/// );
/// ```
pub fn disassemble(heap: &Heap, root: &Proto) -> String {
    let mut out = String::new();
    let mut stack: Vec<(&Proto, String)> = vec![(root, root_label(root))];
    let mut first = true;

    while let Some((proto, label)) = stack.pop() {
        if !first {
            out.push('\n');
        }
        first = false;
        one_proto(heap, proto, &label, &mut out);
        // Reversed push so children list in index order under a LIFO stack.
        for (i, child) in proto.protos.iter().enumerate().rev() {
            stack.push((child.as_ref(), child_label(&label, i)));
        }
    }
    out
}

fn one_proto(heap: &Heap, proto: &Proto, label: &str, out: &mut String) {
    // The label is the position in the tree — unique by construction. A child's own name
    // is an annotation, not an identity: real compilers name every `named let` body the
    // same thing.
    let name = match &proto.name {
        Some(name) if name != label => format!(" ({name})"),
        _ => String::new(),
    };
    out.push_str(&format!(
        "; proto {label}{name}: nparams={} has_rest={} window={} consts={} upvals={} protos={}\n",
        proto.nparams,
        proto.has_rest,
        proto.max_window,
        proto.consts.len(),
        proto.upvals.len(),
        proto.protos.len(),
    ));
    for (i, desc) in proto.upvals.iter().enumerate() {
        let source = match desc {
            UpvalDesc::ParentLocal(reg) => format!("parent local r{reg}"),
            UpvalDesc::ParentUpval(upval) => format!("parent upvalue u{upval}"),
        };
        out.push_str(&format!(";   u{i} <- {source}\n"));
    }

    for (pc, &insn) in proto.code.iter().enumerate() {
        out.push_str(&line(heap, proto, pc, insn));
        out.push('\n');
    }
}

/// One listing line: `PPPP  MNEMONIC    operands` padded to [`COMMENT_COL`] when a comment
/// follows. Lines never carry trailing whitespace, so snapshots survive editors.
fn line(heap: &Heap, proto: &Proto, pc: usize, insn: Insn) -> String {
    let (mnemonic, operands, comment) = match insn.opcode() {
        Some(op) => {
            let (operands, comment) = render(heap, proto, pc, op, insn);
            (op.mnemonic(), operands, comment)
        }
        None => (
            "INVALID",
            format!("{:#04x}", insn.op()),
            format!("raw {:#010x}", insn.0),
        ),
    };

    let mut text = format!("{pc:04}  {mnemonic:<12}{operands}");
    if comment.is_empty() {
        return text.trim_end().to_string();
    }
    if text.len() < COMMENT_COL {
        text.push_str(&" ".repeat(COMMENT_COL - text.len()));
    } else {
        text.push_str("  ");
    }
    text.push_str("; ");
    text.push_str(&comment);
    text
}

/// The operand text and comment for one decoded instruction.
fn render(heap: &Heap, proto: &Proto, pc: usize, op: Op, insn: Insn) -> (String, String) {
    let (a, b, c) = (insn.a(), insn.b(), insn.c());
    let konst = |index: u32| match proto.consts.get(index as usize) {
        Some(&v) => const_text(heap, v),
        None => "k out of bounds".to_string(),
    };
    let target = |offset: i16| {
        let t = pc as i64 + 1 + i64::from(offset);
        if t >= 0 && (t as usize) < proto.code.len() {
            format!("-> {t:04}")
        } else {
            format!("-> out of bounds ({t})")
        }
    };

    match op {
        Op::Move => (format!("r{a}, r{b}"), String::new()),
        Op::LoadK => (format!("r{a}, k{}", insn.bx()), konst(u32::from(insn.bx()))),
        Op::LoadKx => {
            let comment = match proto.code.get(pc + 1) {
                Some(next) if next.opcode() == Some(Op::ExtraArg) => {
                    format!("k{} = {}", next.ax(), konst(next.ax()))
                }
                _ => "missing EXTRAARG".to_string(),
            };
            (format!("r{a}"), comment)
        }
        Op::LoadImm => match singleton_name(insn.bx()) {
            Some(name) => (format!("r{a}, {name}"), String::new()),
            None => (
                format!("r{a}, {}", insn.bx()),
                "invalid singleton".to_string(),
            ),
        },
        Op::LoadI => (format!("r{a}, {}", insn.sbx()), String::new()),

        Op::Add | Op::Sub | Op::Mul | Op::Div | Op::Quot => {
            (format!("r{a}, r{b}, r{c}"), String::new())
        }
        Op::Neg => (format!("r{a}, r{b}"), String::new()),
        Op::AddI => (format!("r{a}, r{b}, {}", insn.sc()), String::new()),

        Op::NumEq | Op::NumLt | Op::NumLe | Op::Eq | Op::Eqv => {
            let pred = match op {
                Op::NumEq => "=",
                Op::NumLt => "<",
                Op::NumLe => "<=",
                Op::Eq => "eq?",
                _ => "eqv?",
            };
            (
                format!("r{a}, r{b}, {c}"),
                skip_comment(c, &format!("({pred} r{a} r{b})")),
            )
        }
        Op::Test => (format!("r{a}, {c}"), skip_comment(c, &format!("r{a}"))),
        Op::TypeP => (format!("r{a}, {c}"), format!("skip unless type {c}")),

        Op::Jmp => {
            let operands = if a == 0 {
                format!("{:+}", insn.sbx())
            } else {
                format!("{:+}, close>=r{}", insn.sbx(), a - 1)
            };
            (operands, target(insn.sbx()))
        }
        Op::ExtraArg => (format!("{}", insn.ax()), String::new()),
        Op::JmpIdx => (format!("r{a}, r{b}, r{c}"), String::new()),

        Op::Cons => (format!("r{a}, r{b}, r{c}"), String::new()),
        Op::Car | Op::Cdr | Op::SetCar | Op::SetCdr => (format!("r{a}, r{b}"), String::new()),
        Op::Cadr => (format!("r{a}, r{b}, {c}"), String::new()),

        Op::Call => (
            format!("r{a}, {b}, {c}"),
            format!("{}, {}", args_text(b), results_text(c)),
        ),
        Op::TailCall => (format!("r{a}, {b}"), args_text(b)),
        Op::Return => {
            let comment = match b {
                0 => format!("all values from r{a}"),
                1 => "no values".to_string(),
                _ => counted(b - 1, "value"),
            };
            (format!("r{a}, {b}"), comment)
        }
        Op::Return1 => (format!("r{a}"), String::new()),
        Op::Apply => (
            format!("r{a}, {b}"),
            format!("{}, last spread", args_text(b)),
        ),

        Op::Closure => {
            let comment = match proto.protos.get(usize::from(insn.bx())) {
                Some(child) => child.name.clone().unwrap_or_default(),
                None => "p out of bounds".to_string(),
            };
            (format!("r{a}, p{}", insn.bx()), comment)
        }
        Op::GetUpval => (
            format!("r{a}, u{b}"),
            upval_comment(proto, b, String::new()),
        ),
        Op::SetUpval => (
            format!("r{a}, u{b}"),
            upval_comment(proto, b, format!("u{b} := r{a}")),
        ),
        Op::GetGlobal | Op::SetGlobal => {
            (format!("r{a}, k{}", insn.bx()), konst(u32::from(insn.bx())))
        }
        Op::GetLocalN => (format!("r{a}, r{b}"), String::new()),
        Op::CloseUpvals => (format!("r{a}"), String::new()),

        Op::VecRef | Op::VecSet | Op::NewVec => (format!("r{a}, r{b}, r{c}"), String::new()),
        Op::PrimCall => (
            format!("r{a}, {b}, {c}"),
            format!("prim {c}, {}", args_text(b)),
        ),

        Op::CaptureCc => (format!("r{a}"), String::new()),
        Op::WindPush => (format!("r{a}, r{b}"), String::new()),
        Op::WindPop | Op::HandlerPop => (String::new(), String::new()),
        Op::HandlerPush => (format!("r{a}, {:+}", insn.sbx()), target(insn.sbx())),
        Op::Raise => {
            let comment = if b == 1 { "continuable" } else { "" };
            (format!("r{a}, {b}"), comment.to_string())
        }
    }
}

/// The skip-family direction, spelled out: skip fires when the condition ≠ the k flag,
/// so k=0 skips when the condition holds and k=1 skips when it does not. This single bit
/// is the most easily inverted one in the ISA; making it words in the frozen text means
/// a polarity regression shows up as a snapshot diff, not an unchanged digit.
fn skip_comment(k: u8, condition: &str) -> String {
    match k {
        0 => format!("skip if {condition}"),
        1 => format!("skip unless {condition}"),
        _ => format!("invalid flag {k}"),
    }
}

fn upval_comment(proto: &Proto, b: u8, in_bounds: String) -> String {
    if usize::from(b) < proto.upvals.len() {
        in_bounds
    } else {
        "u out of bounds".to_string()
    }
}

fn args_text(b: u8) -> String {
    match b {
        0 => "varargs".to_string(),
        _ => counted(b - 1, "arg"),
    }
}

fn results_text(c: u8) -> String {
    match c {
        0 => "all results".to_string(),
        _ => counted(c - 1, "result"),
    }
}

fn counted(n: u8, noun: &str) -> String {
    let plural = if n == 1 { "" } else { "s" };
    format!("{n} {noun}{plural}")
}

/// The `LOADIMM` operand names, keyed by the `value::layout` singleton ordinals so a
/// reordering there re-maps this automatically instead of drifting.
fn singleton_name(bx: u16) -> Option<&'static str> {
    match u64::from(bx) {
        SINGLETON_UNDEFINED => Some("undefined"),
        SINGLETON_UNSPECIFIED => Some("unspecified"),
        SINGLETON_NULL => Some("()"),
        SINGLETON_EOF => Some("eof"),
        SINGLETON_FALSE => Some("#f"),
        SINGLETON_TRUE => Some("#t"),
        _ => None,
    }
}

/// A constant, rendered by content — never by address.
///
/// This is a listing aid, not the R7RS `write` (which arrives in M3): flonums print as
/// Rust's `Debug` text, and compound heap values print opaquely rather than recursively,
/// because a constant pool can hold a cyclic quoted structure.
fn const_text(heap: &Heap, v: Value) -> String {
    if let Some(n) = v.as_fixnum() {
        return n.to_string();
    }
    if let Some(x) = v.as_flonum() {
        return format!("{x:?}");
    }
    if let Some(c) = v.as_char() {
        return format!("#\\{c}");
    }
    if v.is_singleton() {
        for (value, name) in [
            (Value::UNDEFINED, "undefined"),
            (Value::UNSPECIFIED, "unspecified"),
            (Value::NIL, "()"),
            (Value::EOF, "eof"),
            (Value::FALSE, "#f"),
            (Value::TRUE, "#t"),
        ] {
            if v == value {
                return name.to_string();
            }
        }
    }
    match heap.tag_of(v) {
        Some(HeapTag::Symbol) => heap
            .get::<Symbol>(v)
            .map_or_else(|| "#<symbol>".to_string(), |s| s.name.to_string()),
        Some(HeapTag::Str) => heap
            .get::<Str>(v)
            .map_or_else(|| "#<string>".to_string(), |s| format!("{:?}", s.chars)),
        Some(HeapTag::Bignum) => heap
            .get::<Bignum>(v)
            .map_or_else(|| "#<bignum>".to_string(), |b| b.value.to_string()),
        Some(HeapTag::Pair) => "#<pair>".to_string(),
        Some(HeapTag::Vector) => "#<vector>".to_string(),
        Some(HeapTag::Bytevector) => "#<bytevector>".to_string(),
        Some(HeapTag::Closure) => "#<closure>".to_string(),
        Some(HeapTag::UpvalueCell) => "#<upvalue>".to_string(),
        Some(HeapTag::Record) => "#<record>".to_string(),
        Some(HeapTag::RecordType) => "#<record-type>".to_string(),
        None => "#<invalid>".to_string(),
    }
}
