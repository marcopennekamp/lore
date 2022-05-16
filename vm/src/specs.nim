import std/strutils
import std/terminal

import definitions
from evaluator import nil
from utils import with_frame_mem

# TODO (specs): When executing specs, sort them by module name and create a new "group" for each module.

type
  SpecAssertionError* = object of CatchableError
    ## This error is thrown by the intrinsic `lore.test.assert` and caught during spec execution.

proc run_tests*(universe: Universe)

########################################################################################################################
# Tests.                                                                                                               #
########################################################################################################################

type
  SpecTestResult = ref object
    is_success: bool
    error_message: string

let test_success: SpecTestResult = SpecTestResult(is_success: true, error_message: "")

proc new_test_failure(error_message: string): SpecTestResult = SpecTestResult(is_success: false, error_message: error_message)

proc run_test(spec: Spec, frame_mem: pointer): SpecTestResult =
  try:
    discard evaluator.evaluate(addr spec.executable, frame_mem)
    test_success
  except SpecAssertionError as e:
    new_test_failure(e.msg)
  except:
    new_test_failure("Nim exception: " & get_current_exception_msg())

proc print_test_result(spec: Spec, result: SpecTestResult) =
  if result.is_success:
    styled_echo fg_green, "okay: ", reset_style, spec.module_name, " ", spec.description
  else:
    styled_echo fg_red, "fail: ", reset_style, spec.module_name, " ", spec.description
    echo "  ", result.error_message.replace("\n", "\n  ")

proc run_tests*(universe: Universe) =
  with_frame_mem(proc (frame_mem: pointer) =
    for spec in universe.specs:
      if spec.is_test:
        let result = run_test(spec, frame_mem)
        print_test_result(spec, result)
  )

########################################################################################################################
# Benchmarks.                                                                                                          #
########################################################################################################################

# TODO (specs): Implement spec benchmarking.

########################################################################################################################
# Module name filtering.                                                                                               #
########################################################################################################################

# TODO (specs): Implement spec paths.
