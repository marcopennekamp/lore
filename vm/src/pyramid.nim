from definitions import FramePtr, Intrinsic, IntrinsicFunction
from evaluator import nil
import imseqs
import values

proc core_panic(): TaggedValue =
  quit("Panic: The Lore program was forced to terminate.")

proc strings_length(tagged_string: TaggedValue): TaggedValue =
  let string = untag_reference(tagged_string, StringValue)
  tag_int(string.string.len)

proc lists_each(frame: FramePtr, tagged_list: TaggedValue, tagged_function: TaggedValue): TaggedValue =
  let list = untag_reference(tagged_list, ListValue)
  let function = untag_reference(tagged_function, FunctionValue)
  for element in list.elements:
    discard evaluator.evaluate(function, frame, element)
  values.unit_tagged

proc io_println(value: TaggedValue): TaggedValue =
  echo value
  values.unit_tagged

template nullary(arg_name, arg_function): untyped = Intrinsic(name: arg_name, is_frame_aware: false, function: IntrinsicFunction(nullary: arg_function))
template unary(arg_name, arg_function): untyped = Intrinsic(name: arg_name, is_frame_aware: false, function: IntrinsicFunction(unary: arg_function))
template unary_fa(arg_name, arg_function): untyped = Intrinsic(name: arg_name, is_frame_aware: true, function: IntrinsicFunction(unary_fa: arg_function))
template binary(arg_name, arg_function): untyped = Intrinsic(name: arg_name, is_frame_aware: false, function: IntrinsicFunction(binary: arg_function))
template binary_fa(arg_name, arg_function): untyped = Intrinsic(name: arg_name, is_frame_aware: true, function: IntrinsicFunction(binary_fa: arg_function))

let intrinsics*: seq[Intrinsic] = @[
  nullary("lore.core.panic", core_panic),
  unary("lore.strings.length", strings_length),
  binary_fa("lore.lists.each", lists_each),
  unary("lore.io.println", io_println),
]
