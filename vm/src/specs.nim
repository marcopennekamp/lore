import std/strutils
import std/sugar
import std/terminal

import definitions
from evaluator import nil
import imseqs
import time
from utils import with_frame_mem, get_terminal_padding_space
import values

type
  SpecAssertionError* = object of CatchableError
    ## This error is thrown by the intrinsic `lore.test.assert` and caught during spec execution.

  ModuleNameFilter* = distinct ImSeq[string]

proc run_tests*(universe: Universe, module_name_filter: ModuleNameFilter)

########################################################################################################################
# Spec purposes.                                                                                                       #
########################################################################################################################

type SpecPurpose {.pure.} = enum Test, Benchmark

proc has_purpose(spec: Spec, purpose: SpecPurpose): bool =
  ## Whether `spec` has the given purpose. A test and benchmark spec has both purposes.
  case purpose
  of SpecPurpose.Test: spec.is_test
  of SpecPurpose.Benchmark: spec.is_benchmark

########################################################################################################################
# Module name filters.                                                                                                 #
########################################################################################################################

template names(filter: ModuleNameFilter): ImSeq[string] = cast[ImSeq[string]](filter)

proc contains(filter: ModuleNameFilter, spec: Spec): bool =
  if filter.names.len == 0: true
  else:
    for module_name in filter.names:
      if spec.module_name.starts_with(module_name):
        return true
    false

########################################################################################################################
# Spec groups.                                                                                                         #
########################################################################################################################

type SpecGroup = ref object
  ## A spec group combines all specs of the same module.
  module_name: string
  specs: seq[Spec]

iterator spec_groups(universe: Universe, purpose: SpecPurpose, module_name_filter: ModuleNameFilter): SpecGroup =
  ## A spec group view on the specs of `universe`. The specs already have to be sorted by module name, so the resulting
  ## spec groups will also be sorted by default. Specs are filtered by `purpose` and `module_name_filter`. Spec groups
  ## without a single included spec aren't yielded.
  if universe.specs.len > 0:
    # The algorithm here is quite simple: If the last module name changes, yield a spec group and reset the working
    # list of `specs`. `emit` takes into account that `specs` might be empty even after a change to the last module
    # name, as all specs in a module might be excluded by the filters.
    var specs: seq[Spec]
    var last_module_name = universe.specs[0].module_name

    template emit(reset_length: bool) =
      if specs.len > 0:
        yield SpecGroup(module_name: last_module_name, specs: specs)
        if reset_length: specs.set_len(0)

    for spec in universe.specs:
      if last_module_name != spec.module_name:
        emit(true)
        last_module_name = spec.module_name
      if spec.has_purpose(purpose) and spec in module_name_filter:
        specs.add(spec)
    emit(false)

proc print_header(spec_group: SpecGroup) =
  echo spec_group.module_name, ":"

proc print_footer(spec_group: SpecGroup) =
  echo ""

########################################################################################################################
# Spec execution commons.                                                                                              #
########################################################################################################################

type
  SpecExecutionResult = ref object of RootObj
    spec: Spec

  SpecExecutionFailure = ref object of SpecExecutionResult
    error_message: string

  SpecTestSuccess = ref object of SpecExecutionResult

  SpecBenchmarkSuccess = ref object of SpecExecutionResult
    time_per_op: int64
      ## The time taken per iteration in nanoseconds.

  SpecExecutionStats = ref object
    total_specs: int
    successful_specs: int
    failed_specs: int
    total_time: int64
      ## The total time taken to run all specs in nanoseconds.

proc new_spec_failure(spec: Spec, error_message: string): SpecExecutionFailure =
  SpecExecutionFailure(spec: spec, error_message: error_message)

proc new_test_success(spec: Spec): SpecTestSuccess =
  SpecTestSuccess(spec: spec)

proc new_benchmark_success(spec: Spec, time_per_op: int64): SpecBenchmarkSuccess =
  SpecBenchmarkSuccess(spec: spec, time_per_op: time_per_op)

method is_success(res: SpecExecutionResult): bool {.base.} = false
method is_success(res: SpecTestSuccess): bool = true
method is_success(res: SpecBenchmarkSuccess): bool = true

method print(res: SpecExecutionResult) {.base.} = quit("Cannot print base SpecExecutionResult.")

method print(failure: SpecExecutionFailure) =
  styled_echo "- ", fg_red, "fail: ", reset_style, failure.spec.description
  echo "    ", failure.error_message.replace("\n", "\n    ")

method print(success: SpecTestSuccess) =
  styled_echo "- ", fg_green, "okay: ", reset_style, success.spec.description

method print(success: SpecBenchmarkSuccess) =
  # We're using the padding to right-align the "time per op" strings.
  let time_per_op_string = format_time_per_op(success.time_per_op)
  let padding = get_terminal_padding_space("- okay: ".len + success.spec.description.len, time_per_op_string.len)
  styled_echo "- ", fg_green, "okay: ", reset_style, success.spec.description, padding, time_per_op_string

proc update(stats: SpecExecutionStats, result: SpecExecutionResult) =
  stats.total_specs += 1
  if result.is_success: stats.successful_specs += 1
  else: stats.failed_specs += 1

proc print_summary(stats: SpecExecutionStats, purpose: SpecPurpose) =
  let heading =
    case purpose
    of SpecPurpose.Test: "Tests:"
    of SpecPurpose.Benchmark: "Benchmarks:"
  let color = if stats.failed_specs == 0: fg_green else: fg_red
  styled_echo color, heading, " ",
              "total ", $stats.total_specs, ", ",
              "succeeded ", $stats.successful_specs, ", ",
              "failed ", $stats.failed_specs, " ",
              "(completed in ", stats.total_time.to_readable_time, ")"

template try_spec_run(code: untyped): SpecExecutionResult =
  ## Wraps the spec execution in a `try`, catching assertion errors to produce `SpecExecutionFailures`. `code` should
  ## produce a `SpecExecutionResult`.
  try:
    code
  except SpecAssertionError as e:
    new_spec_failure(spec, e.msg)
  except:
    new_spec_failure(spec, "Nim exception: " & get_current_exception_msg())

proc run_specs(
  universe: Universe,
  module_name_filter: ModuleNameFilter,
  purpose: SpecPurpose,
  run_spec: (spec: Spec, frame_mem: pointer) -> SpecExecutionResult,
) =
  ## `run_specs` is an implementation of both `run_tests` and `run_benchmarks`.
  var stats = SpecExecutionStats(total_specs: 0, successful_specs: 0, failed_specs: 0, total_time: 0)
  with_frame_mem(proc (frame_mem: pointer) =
    stats.total_time = timed:
      for spec_group in universe.spec_groups(purpose, module_name_filter):
        spec_group.print_header()
        for spec in spec_group.specs:
          let res = run_spec(spec, frame_mem)
          stats.update(res)
          res.print()
        spec_group.print_footer()
  )
  stats.print_summary(purpose)

########################################################################################################################
# Tests.                                                                                                               #
########################################################################################################################

proc run_test(spec: Spec, frame_mem: pointer): SpecExecutionResult =
  try_spec_run:
    discard evaluator.evaluate(addr spec.executable, frame_mem)
    new_test_success(spec)

proc run_tests*(universe: Universe, module_name_filter: ModuleNameFilter) =
  run_specs(universe, module_name_filter, SpecPurpose.Test, run_test)

########################################################################################################################
# Benchmarks.                                                                                                          #
########################################################################################################################

var benchmark_bucket: TaggedValue = tag_reference(nil)

proc run_benchmark(spec: Spec, frame_mem: pointer): SpecExecutionResult =
  try_spec_run:
    let time_per_op = benchmark:
      benchmark_bucket = evaluator.evaluate(addr spec.executable, frame_mem)
    new_benchmark_success(spec, time_per_op)

proc run_benchmarks*(universe: Universe, module_name_filter: ModuleNameFilter) =
  run_specs(universe, module_name_filter, SpecPurpose.Benchmark, run_benchmark)
