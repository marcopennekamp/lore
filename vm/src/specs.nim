import std/strutils
import std/terminal

import definitions
from evaluator import nil
import imseqs
from utils import with_frame_mem

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
# Tests.                                                                                                               #
########################################################################################################################

type
  SpecTestResult = ref object
    spec: Spec
    is_success: bool
    error_message: string

  SpecTestStats = ref object
    total_tests: int
    successful_tests: int
    failed_tests: int

proc new_test_success(spec: Spec): SpecTestResult =
  SpecTestResult(spec: spec, is_success: true, error_message: "")

proc new_test_failure(spec: Spec, error_message: string): SpecTestResult =
  SpecTestResult(spec: spec, is_success: false, error_message: error_message)

proc print(result: SpecTestResult) =
  if result.is_success:
    styled_echo "- ", fg_green, "okay: ", reset_style, result.spec.description
  else:
    styled_echo "- ", fg_red, "fail: ", reset_style, result.spec.description
    echo "    ", result.error_message.replace("\n", "\n    ")

proc update(stats: SpecTestStats, result: SpecTestResult) =
  stats.total_tests += 1
  if result.is_success:
    stats.successful_tests += 1
  else:
    stats.failed_tests += 1

proc print_summary(stats: SpecTestStats) =
  let color = if stats.failed_tests == 0: fg_green else: fg_red
  styled_echo color, "Tests: total ", $stats.total_tests, ", ",
              "succeeded ", $stats.successful_tests, ", ",
              "failed ", $stats.failed_tests

proc run_test(spec: Spec, frame_mem: pointer): SpecTestResult =
  try:
    discard evaluator.evaluate(addr spec.executable, frame_mem)
    new_test_success(spec)
  except SpecAssertionError as e:
    new_test_failure(spec, e.msg)
  except:
    new_test_failure(spec, "Nim exception: " & get_current_exception_msg())

proc run_tests*(universe: Universe, module_name_filter: ModuleNameFilter) =
  var stats = SpecTestStats(total_tests: 0, successful_tests: 0, failed_tests: 0)
  with_frame_mem(proc (frame_mem: pointer) =
    for spec_group in universe.spec_groups(SpecPurpose.Test, module_name_filter):
      spec_group.print_header()
      for spec in spec_group.specs:
        let result = run_test(spec, frame_mem)
        stats.update(result)
        result.print()
      spec_group.print_footer()
  )
  stats.print_summary()

########################################################################################################################
# Benchmarks.                                                                                                          #
########################################################################################################################

# TODO (specs): Implement spec benchmarking.
