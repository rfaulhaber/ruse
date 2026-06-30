//! The Ruse register VM (RBC-1).
//!
//! Inspired by Lua's register-based VM
//! (<https://www.mcours.net/cours/pdf/hasclic3/hasssclic818.pdf>). Each call frame owns a
//! window of up to **250** registers (`R0`–`R249`) within a flat per-fiber register array.
//! 8-bit operand fields address registers 0–255; values 250–255 are reserved for future
//! addressing-mode escapes.
//!
//! The instruction set (50 opcodes) is specified in `ruse-bytecode-spec.md`. Nothing is
//! implemented yet — see `docs/ROADMAP.md` (milestones M2–M3) for the build order.
