import std/times
import std/monotimes
import std/strutils

########################################################################################################################
# Time.                                                                                                                #
########################################################################################################################

proc to_microseconds*(ns: int64): float64 = float64(ns) / 1_000.0
proc to_milliseconds*(ns: int64): float64 = float64(ns) / 1_000_000.0
proc to_seconds*(ns: int64): float64 = float64(ns) / 1_000_000_000.0

proc to_nanoseconds*(seconds: float64): int64 = int64(seconds * 1_000_000_000.0)

proc to_readable_time*(ns: int64, format_threshold: int64 = 10_000): string =
  ## Converts `ns` into a readable string of nanoseconds, microseconds, milliseconds, or seconds depending on its
  ## magnitude. All times from microseconds up are reported with a single digit fraction.
  ##
  ## `format_threshold` determines the minimum nanoseconds value that should be formatted to microseconds or up.
  template format1(time: float64): string = time.format_float(ffDecimal, 1)
  if ns < format_threshold: $ns & "ns"
  elif ns < 10_000 * 1000: format1(to_microseconds(ns)) & "µs"
  elif ns < 500_000_000: format1(to_milliseconds(ns)) & "ms"
  else: format1(to_seconds(ns)) & "s"

template timed*(code: untyped): int64 =
  ## Times `code` with mono time and returns the time taken in nanoseconds.
  var time: int64 = 0
  block:
    let t0 = get_mono_time()
    code
    time = in_nanoseconds(get_mono_time() - t0)
  time

########################################################################################################################
# Benchmarking.                                                                                                        #
########################################################################################################################

# All times are in seconds.
const ideal_warmup_time: float64 = 2.0
const ideal_benchmark_time: float64 = 10.0
const warmup_iteration_interval = 250

template benchmark*(code: untyped): int64 =
  ## Benchmarks `code` and returns the time per operation in nanoseconds. A benchmark is first executed in a warmup
  ## phase for 2 seconds and then for a number of iterations that will roughly take 10 seconds.
  ##
  ## Note that this template is quite complex and thought should be given to code size when using it.
  var time_per_op: int64 = 0
  block:
    let warmup_t0 = get_mono_time()
    let warmup_end = warmup_t0 + init_duration(nanoseconds = ideal_warmup_time.to_nanoseconds)
    var iterations: int64 = 0
    while get_mono_time() < warmup_end:
      # Execute the code in batches to minimize the effect of the warmup time calculations on the iteration count,
      # especially for small benchmarks.
      for i in 0 ..< warmup_iteration_interval:
        code
      iterations += warmup_iteration_interval

    # Note that we cannot use `warmup_end` here, because a single run of the `for` loop might have taken significantly
    # more than the alotted ideal time.
    let warmup_time = in_nanoseconds(get_mono_time() - warmup_t0).to_seconds

    # We have to correct for the fact that the `while` loop might not catch the end time exactly when the `for` loop
    # terminates. For example, if the 250 iterations in the nested loop take for example 3.5 seconds, and we want to
    # execute 10 seconds worth of iterations, we first correct 250 to 142.857 (for 2 seconds), and then muliply that up
    # to 714 using the difference between the ideal benchmark and warmup times.
    let warmup_correction_factor = warmup_time / ideal_warmup_time
    iterations = int64((float64(iterations) / warmup_correction_factor) * (ideal_benchmark_time / ideal_warmup_time))

    let time_ns = timed:
      for i in 0 ..< iterations:
        code
    time_per_op = time_ns div iterations
  time_per_op

proc format_time_per_op*(time_per_op: int64): string =
  ## Formats `time_per_op` into a readable time per op. The formatting threshold is higher than the default for
  ## `to_readable_time` so that most operations that complete in reasonable time are reported in nanoseconds for
  ## consistency in benchmark reports.
  $time_per_op.to_readable_time(1_000_000) & "/op"

template benchmark_and_print*(benchmark_name: string, code: untyped) =
  let time_per_op = benchmark:
    code
  echo benchmark_name, ": ", format_time_per_op(time_per_op)
