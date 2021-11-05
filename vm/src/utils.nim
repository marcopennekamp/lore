import times, os, sugar

const is_release* = defined(release) or defined(danger)

# Executes the given code if the build is in release mode.
macro when_release*(code: untyped): untyped =
  when is_release:
    code
  else:
    discard

# Executes the given code if the build is not in release mode.
macro when_debug*(code: untyped): untyped =
  when not is_release:
    code
  else:
    discard

## Executes the given function with newly allocated frame memory. The allocated memory is automatically freed after
## `f` finishes. `with_frame_mem` allocates memory on the Boehm GC heap when the Boehm GC is in use. This allows the GC
## to discover pointers to values which are only referenced from a frame register.
proc with_frame_mem*(f: (pointer) -> void) =
  let frame_mem: pointer = alloc0(sizeof(uint64) * 250_000)
  f(frame_mem)
  dealloc(frame_mem)

template benchmark*(benchmark_name: string, runs: int, code: untyped) =
  block:
    let t0 = epoch_time()
    var i: int = 0
    while i < runs:
      code
      i += 1
    let elapsed = epoch_time() - t0
    let elapsed_ns = elapsed * 1_000_000_000
    let per_run = uint(elapsed_ns / runs.float)
    echo benchmark_name, ": ", per_run, "ns/op"
