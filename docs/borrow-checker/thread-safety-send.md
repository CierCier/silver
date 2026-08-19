# Concurrency & Send Safety

Silver incorporates compile-time concurrency checking (`semantic/send_check.rs`) to prevent data races and dangling stack references when launching asynchronous tasks.

---

## 1. Concurrency Boundaries (`launch`)

Silver's concurrency model allows spawning asynchronous tasks via the `launch` keyword. When arguments or captures are transferred to a background task:

```silver
struct Message {
    i64 id;
    u8* payload;
}

void worker(Message msg) {
    // runs in separate OS thread
}

i32 main() {
    Message m;
    m.id = 1;
    m.payload = (u8*)malloc(64);

    // Spawns background worker
    launch worker(move m);
    return 0;
}
```

---

## 2. Structural Send Verification (`semantic/send_check.rs`)

The compiler performs structural type checking (`structural_send`) on every argument passed to `launch`:

### Rules for Send Safety:

1. **No Stack References Across Thread Boundaries**:
   Passing `&T` or `&mut T` to a background task is strictly rejected. Because the spawned task runs concurrently and outlives the caller's stack frame, borrowing caller stack memory would cause data races and use-after-free conditions.

   ```silver
   void worker_ref(&Message msg) { ... }

   i32 main() {
       Message m;
       // COMPILE ERROR: cannot pass reference '&Message' across thread boundary
       launch worker_ref(&m);
   }
   ```

2. **Owned Value Transfers**:
   Only owned values (`T`) with Send-safe layouts (or types implementing `Send`) may be moved into `launch` tasks.

3. **Raw Pointer Checks**:
   Raw pointers (`T*`) passed to tasks are checked against concurrency rules to prevent sharing un-synchronized mutable state.

---

## 3. Interaction with Move & Drop Semantics

- Moving a value into a `launch` call marks the local variable as moved in the caller frame (`level = 2`).
- The background thread takes full ownership of the resource and drops it upon task completion.
- Attempting to access the variable in the caller frame after `launch` triggers a use-after-move compiler error.
