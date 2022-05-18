import std/strutils
import std/sugar
import std/terminal

const is_release* = defined(release) or defined(danger)

macro when_release*(code: untyped): untyped =
  ## Executes the given code if the build is in release mode.
  when is_release:
    code

macro when_debug*(code: untyped): untyped =
  ## Executes the given code if the build is not in release mode.
  when not is_release:
    code

proc with_frame_mem*(f: (pointer) -> void) =
  ## Executes the given function with newly allocated frame memory. The allocated memory is automatically freed after
  ## `f` finishes. `with_frame_mem` allocates memory on the Boehm GC heap when the Boehm GC is in use. This allows the
  ## GC to discover pointers to values which are only referenced from a frame register.
  let frame_mem: pointer = alloc0(sizeof(uint64) * 250_000)
  f(frame_mem)
  dealloc(frame_mem)

template call_if_any_exists*(function, arg0, default0, arg1, default1, default_result): untyped =
  ## Calls `function` with `arg1` and `arg2` if either is not `nil`. Any `nil` argument is replaced with the default.
  ## Otherwise, returns the `default_result`.
  if arg0 !== nil or arg1 !== nil:
    if arg0 === nil: arg0 = default0
    if arg1 === nil: arg1 = default1
    function(arg0, arg1)
  else: default_result

template offset_addr*(value: untyped, offset: int): pointer = cast[pointer](cast[int](addr value) +% offset)

proc calculate_terminal_padding*(left_length: int, right_length: int): int =
  ## Calculates the available padding between two strings if both strings were printed to the terminal given its
  ## current width. The padding will always be at least 1, even if the resulting line would exceed the terminal width.
  max(terminal_width() - left_length - right_length, 1)

proc get_terminal_padding_space*(left_length: int, right_length: int): string =
  ## Calculates the required padding with `compute_terminal_padding` and returns a string of the exact length.
  " ".repeat(calculate_terminal_padding(left_length, right_length))
