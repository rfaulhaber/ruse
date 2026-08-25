//! The global environment: one append-only vector of stable slots.
//!
//! R7RS §5.4 requires a top-level redefinition to take retroactive effect, so `GETGLOBAL`
//! can never be compiled down to a direct address — every access goes through exactly one
//! indirection: symbol → slot index → current value. Slots are append-only and never move,
//! which is what will let a later link pass cache slot indices without a correctness risk.
//!
//! The map is keyed by the symbol's encoded bits. That is sound because the compiler only
//! ever emits *interned* symbols as global keys, and interned symbols are permanent GC
//! roots at stable addresses — the bits of `heap.symbol("x")` never change for the life of
//! the heap.

use std::collections::HashMap;

use crate::gc::Tracer;
use crate::value::Value;

/// One binding: its current value, its name (a symbol, for diagnostics), and whether it is
/// still the pristine definition the runtime installed at boot.
struct Slot {
    value: Value,
    name: Value,
    /// True until user code writes the slot. The compiler's primitive-inlining policy
    /// keys on this: `(+ a b)` compiles to `ADD` only while `+` is the runtime's own
    /// binding, and falls back to a real global call the moment anyone redefines it.
    pristine_builtin: bool,
}

/// The append-only global slot table.
#[derive(Default)]
pub struct Globals {
    slots: Vec<Slot>,
    /// Symbol bits → slot index.
    index: HashMap<u64, u32>,
}

impl Globals {
    /// The slot named by `sym`, if one has ever been created.
    pub fn resolve(&self, sym: Value) -> Option<u32> {
        self.index.get(&sym.to_bits()).copied()
    }

    /// The slot named by `sym`, created unbound (holding `undefined`) on first sight.
    pub fn intern(&mut self, sym: Value) -> u32 {
        if let Some(slot) = self.resolve(sym) {
            return slot;
        }
        let slot = u32::try_from(self.slots.len()).unwrap_or(u32::MAX);
        self.slots.push(Slot {
            value: Value::UNDEFINED,
            name: sym,
            pristine_builtin: false,
        });
        self.index.insert(sym.to_bits(), slot);
        slot
    }

    /// The value in `slot`. `Value::UNDEFINED` means the slot exists but is unbound.
    pub fn get(&self, slot: u32) -> Value {
        self.slots
            .get(slot as usize)
            .map_or(Value::UNDEFINED, |s| s.value)
    }

    /// Write `slot` on behalf of user code (`SETGLOBAL`, top-level `define`). Clears the
    /// pristine flag: from here on the compiler must treat this name as an ordinary global.
    pub fn set(&mut self, slot: u32, value: Value) {
        if let Some(s) = self.slots.get_mut(slot as usize) {
            s.value = value;
            s.pristine_builtin = false;
        }
    }

    /// Install a boot-time binding and mark it pristine. Used only while the runtime sets
    /// up its own procedures; user writes go through [`Globals::set`].
    pub fn define_builtin(&mut self, sym: Value, value: Value) {
        let slot = self.intern(sym);
        if let Some(s) = self.slots.get_mut(slot as usize) {
            s.value = value;
            s.pristine_builtin = true;
        }
    }

    /// Whether `sym` still names the untouched boot-time binding. This is the compiler's
    /// licence to inline: emit `ADD` for `+`, or `PRIMCALL` for a known native, only while
    /// this holds — otherwise the call compiles as `GETGLOBAL` + `CALL` so a redefinition
    /// is honoured.
    pub fn is_pristine_builtin(&self, sym: Value) -> bool {
        self.resolve(sym)
            .and_then(|slot| self.slots.get(slot as usize))
            .is_some_and(|s| s.pristine_builtin)
    }

    /// Convenience lookup: the bound value of `sym`, if bound.
    pub fn lookup_value(&self, sym: Value) -> Option<Value> {
        let v = self.get(self.resolve(sym)?);
        if v.is_undefined() { None } else { Some(v) }
    }

    /// How many slots exist.
    pub fn len(&self) -> usize {
        self.slots.len()
    }

    /// Whether no slot has ever been created.
    pub fn is_empty(&self) -> bool {
        self.slots.is_empty()
    }

    /// Report every value and name this table holds. Called from the VM's root set.
    pub(crate) fn trace_into(&self, tracer: &mut Tracer<'_>) {
        for slot in &self.slots {
            tracer.mark(slot.value);
            tracer.mark(slot.name);
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::gc::Heap;

    #[test]
    fn slots_are_stable_and_appended_once_per_name() {
        let mut heap = Heap::new();
        let mut globals = Globals::default();

        let x = heap.symbol("x");
        let y = heap.symbol("y");

        let sx = globals.intern(x);
        let sy = globals.intern(y);
        assert_ne!(sx, sy);
        assert_eq!(globals.intern(x), sx, "re-interning must not move the slot");
        assert_eq!(globals.len(), 2);

        assert!(globals.get(sx).is_undefined(), "fresh slots are unbound");
        globals.set(sx, Value::TRUE);
        assert_eq!(globals.get(sx), Value::TRUE);
        assert_eq!(globals.resolve(y), Some(sy));
        assert_eq!(globals.resolve(heap.symbol("z")), None);
    }

    #[test]
    fn user_writes_clear_the_pristine_flag() {
        let mut heap = Heap::new();
        let mut globals = Globals::default();
        let plus = heap.symbol("+");

        globals.define_builtin(plus, Value::TRUE);
        assert!(globals.is_pristine_builtin(plus));

        let slot = globals.intern(plus);
        globals.set(slot, Value::FALSE);
        assert!(
            !globals.is_pristine_builtin(plus),
            "a redefined builtin must stop being inlined"
        );
        assert_eq!(globals.lookup_value(plus), Some(Value::FALSE));
    }

    #[test]
    fn unbound_and_absent_are_both_unbound() {
        let mut heap = Heap::new();
        let mut globals = Globals::default();
        let ghost = heap.symbol("ghost");
        assert_eq!(globals.lookup_value(ghost), None);
        globals.intern(ghost);
        assert_eq!(globals.lookup_value(ghost), None);
        assert!(!globals.is_pristine_builtin(ghost));
    }
}
