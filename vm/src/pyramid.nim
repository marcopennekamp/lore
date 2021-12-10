import std/strformat

from definitions import FramePtr, Intrinsic, IntrinsicFunction
from evaluator import nil
import imseqs
import values

proc lists_length(tagged_list: TaggedValue): TaggedValue =
  let list = untag_reference(tagged_list, ListValue)
  tag_int(list.elements.len)

proc lists_get(tagged_list: TaggedValue, tagged_index: TaggedValue): TaggedValue =
  let list = untag_reference(tagged_list, ListValue)
  let index = untag_int(tagged_index)
  if index < 0 or index >= list.elements.len:
    # TODO (vm): Obviously this needs to either return a "nil" constant that can be checked in Lore (to create a None
    #            if the value could not be found) or bubble up an exception, instead of shutting down the VM.
    quit(fmt"Cannot access {list} at index {index}.")
  list.elements[index]

proc lists_each(frame: FramePtr, tagged_list: TaggedValue, tagged_function: TaggedValue): TaggedValue =
  let list = untag_reference(tagged_list, ListValue)
  let function = untag_reference(tagged_function, FunctionValue)
  for element in list.elements:
    discard evaluator.evaluate(function, frame, element)
  values.unit

proc io_println(value: TaggedValue): TaggedValue =
  echo value
  values.unit

template unary(arg_name, arg_function): untyped = Intrinsic(name: arg_name, function: IntrinsicFunction(unary: arg_function))
template unary_fa(arg_name, arg_function): untyped = Intrinsic(name: arg_name, function: IntrinsicFunction(unary_fa: arg_function))
template binary(arg_name, arg_function): untyped = Intrinsic(name: arg_name, function: IntrinsicFunction(binary: arg_function))
template binary_fa(arg_name, arg_function): untyped = Intrinsic(name: arg_name, function: IntrinsicFunction(binary_fa: arg_function))

let intrinsics*: seq[Intrinsic] = @[
  unary("lore.lists.length", lists_length),
  binary("lore.lists.get", lists_get),
  binary_fa("lore.lists.each", lists_each),
  unary("lore.io.println", io_println),
]
