import std/strformat

from definitions import FramePtr, Intrinsic, IntrinsicFunction
from evaluator import nil
import imseqs
import values

proc core_equal(a: TaggedValue, b: TaggedValue): TaggedValue =
  # TODO (assembly): Implement.
  quit("`core_equal` is not yet implemented.")

proc core_less_than(a: TaggedValue, b: TaggedValue): TaggedValue =
  # TODO (assembly): Implement.
  quit("`core_less_than` is not yet implemented.")

proc core_to_string(value: TaggedValue): TaggedValue =
  values.new_string_value_tagged($value)

proc core_panic(message: TaggedValue): TaggedValue =
  quit(fmt"Panic: {message}")

proc int_to_real(value: TaggedValue): TaggedValue =
  let value = untag_int(value)
  new_real_value_tagged(float(value))

proc string_length(tagged_string: TaggedValue): TaggedValue =
  let string = untag_reference(tagged_string, StringValue)
  tag_int(string.string.len)

proc list_each(frame: FramePtr, tagged_list: TaggedValue, tagged_function: TaggedValue): TaggedValue =
  let list = untag_reference(tagged_list, ListValue)
  let function = untag_reference(tagged_function, FunctionValue)
  for element in list.elements:
    discard evaluator.evaluate_function_value(function, frame, element)
  values.unit_value_tagged

proc io_println(value: TaggedValue): TaggedValue =
  echo value
  values.unit_value_tagged

template nullary(arg_name, arg_function): untyped = Intrinsic(name: arg_name, is_frame_aware: false, function: IntrinsicFunction(nullary: arg_function))
template unary(arg_name, arg_function): untyped = Intrinsic(name: arg_name, is_frame_aware: false, function: IntrinsicFunction(unary: arg_function))
template unary_fa(arg_name, arg_function): untyped = Intrinsic(name: arg_name, is_frame_aware: true, function: IntrinsicFunction(unary_fa: arg_function))
template binary(arg_name, arg_function): untyped = Intrinsic(name: arg_name, is_frame_aware: false, function: IntrinsicFunction(binary: arg_function))
template binary_fa(arg_name, arg_function): untyped = Intrinsic(name: arg_name, is_frame_aware: true, function: IntrinsicFunction(binary_fa: arg_function))

let intrinsics*: seq[Intrinsic] = @[
  binary("lore.core.equal?", core_equal),
  binary("lore.core.less_than?", core_less_than),
  unary("lore.core.to_string", core_to_string),
  unary("lore.core.panic", core_panic),

  unary("lore.int.to_real", int_to_real),

  #binary("lore.string.at", string_at),
  unary("lore.string.length", string_length),

  binary_fa("lore.list.each", list_each),

  unary("lore.io.println", io_println),
]
