//! Proof that sweeping runs `Drop`, and not merely `dealloc`.
//!
//! The heap's own accounting cannot demonstrate this: it subtracts whatever `extra_bytes`
//! claims, whether or not the underlying `String`, `Vec` or `BigInt` buffer was actually
//! released. So this test measures the *process* allocator instead. A sweep that freed only
//! the 16-byte header and leaked the payload would sail through every unit test and show up
//! here as a number that never comes back down.
//!
//! This file installs a `#[global_allocator]`, so it deliberately holds one test: it is the
//! whole test binary's allocation that is being measured.

// Integration tests are separate crates, so the crate-root `cfg_attr(test, allow(...))` in
// the library does not reach them. Asserting with `unwrap` is the point of a test.
#![allow(clippy::unwrap_used)]
// A `GlobalAlloc` implementation is unsafe by definition; every method here forwards to
// `System`.
#![allow(unsafe_code)]

use std::alloc::{GlobalAlloc, Layout, System};
use std::sync::atomic::{AtomicUsize, Ordering};

use ruse::Value;
use ruse::gc::Heap;

static LIVE_BYTES: AtomicUsize = AtomicUsize::new(0);

struct Counting;

// SAFETY: every method forwards to `System`, which is a correct allocator; the counters are
// bookkeeping on the side and do not affect the returned pointers.
unsafe impl GlobalAlloc for Counting {
    unsafe fn alloc(&self, layout: Layout) -> *mut u8 {
        LIVE_BYTES.fetch_add(layout.size(), Ordering::Relaxed);
        // SAFETY: `layout` is forwarded unchanged from a valid caller.
        unsafe { System.alloc(layout) }
    }

    unsafe fn dealloc(&self, ptr: *mut u8, layout: Layout) {
        LIVE_BYTES.fetch_sub(layout.size(), Ordering::Relaxed);
        // SAFETY: `ptr` and `layout` are forwarded unchanged from a valid caller.
        unsafe { System.dealloc(ptr, layout) }
    }
}

#[global_allocator]
static ALLOCATOR: Counting = Counting;

/// Roughly 4 MiB of payload per object kind — large enough that the harness's own traffic
/// cannot be mistaken for it.
const CHUNK: usize = 64 * 1024;
const COUNT: usize = 64;
const PAYLOAD: usize = CHUNK * COUNT;

#[test]
fn sweeping_and_dropping_the_heap_both_release_rust_side_storage() {
    let baseline = LIVE_BYTES.load(Ordering::Relaxed);

    let mut heap = Heap::new();

    for _ in 0..COUNT {
        heap.string("x".repeat(CHUNK));
        heap.bytevector(vec![0u8; CHUNK]);
        heap.vector(vec![Value::TRUE; CHUNK / size_of::<Value>()]);
    }
    assert_eq!(heap.live_objects(), COUNT * 3);

    let peak = LIVE_BYTES.load(Ordering::Relaxed);
    assert!(
        peak >= baseline + PAYLOAD * 3,
        "expected at least {} bytes of payload, saw {}",
        PAYLOAD * 3,
        peak - baseline
    );

    // Nothing roots any of it.
    // SAFETY: this is the test's one safepoint, and nothing rooted afterwards is
    // dereferenced — the assertions that follow read process-allocator counters, not the heap.
    let stats = unsafe { heap.collect(&()) };
    assert_eq!(stats.freed, COUNT * 3);
    assert_eq!(heap.live_objects(), 0);

    let after_sweep = LIVE_BYTES.load(Ordering::Relaxed);
    assert!(
        after_sweep < baseline + CHUNK,
        "sweep released only {} of {} bytes: `Drop` is not running on the payloads",
        peak - after_sweep,
        peak - baseline
    );

    // The same obligation applies when a heap is dropped with objects still live.
    for _ in 0..COUNT {
        heap.string("y".repeat(CHUNK));
    }
    assert!(LIVE_BYTES.load(Ordering::Relaxed) >= baseline + PAYLOAD);

    drop(heap);

    let after_drop = LIVE_BYTES.load(Ordering::Relaxed);
    assert!(
        after_drop < baseline + CHUNK,
        "dropping the heap leaked {} bytes",
        after_drop.saturating_sub(baseline)
    );
}
