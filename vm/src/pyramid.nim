import std/strformat
import std/strutils

from definitions import FramePtr, Intrinsic, IntrinsicFunction
from evaluator import nil
import imseqs
import values

type Arguments = open_array[TaggedValue]

template arg(index: int): TaggedValue = arguments[index]

var error_symbol: TaggedValue = TaggedValue(cast[uint64](nil))

proc get_error_symbol(): TaggedValue =
  ## Returns a symbol value `#error`, which is used by various intrinsics to communicate an error to Pyramid. We cannoi
  ## use Option because it's not defined within the VM.
  if untag_reference(error_symbol) == nil:
    error_symbol = new_symbol_value_tagged("error")
  error_symbol

proc core_equal(frame: FramePtr, arguments: Arguments): TaggedValue =
  # TODO (assembly): Implement.
  quit("`core_equal` is not yet implemented.")

proc core_less_than(frame: FramePtr, arguments: Arguments): TaggedValue =
  # TODO (assembly): Implement.
  quit("`core_less_than` is not yet implemented.")

proc core_to_string(frame: FramePtr, arguments: Arguments): TaggedValue =
  values.new_string_value_tagged($arg(0))

proc core_panic(frame: FramePtr, arguments: Arguments): TaggedValue =
  quit(fmt"Panic: {arg(0)}")

proc int_to_real(frame: FramePtr, arguments: Arguments): TaggedValue =
  let value = untag_int(arg(0))
  new_real_value_tagged(float64(value))

proc real_to_int(frame: FramePtr, arguments: Arguments): TaggedValue =
  ## real_to_int(value: Real): Int
  let value = untag_reference(arg(0), RealValue)
  tag_int(int64(value.real))

proc real_parse(frame: FramePtr, arguments: Arguments): TaggedValue =
  ## real_parse(value: String): Real | #error
  let string = untag_reference(arg(0), StringValue).string
  try:
    new_real_value_tagged(parse_float(string))
  except ValueError:
    get_error_symbol()

proc real_is_nan(frame: FramePtr, arguments: Arguments): TaggedValue =
  let value = untag_reference(arg(0), RealValue)
  tag_boolean(value.real == NaN)

proc string_length(frame: FramePtr, arguments: Arguments): TaggedValue =
  let string = untag_reference(arg(0), StringValue)
  tag_int(string.string.len)

proc list_each(frame: FramePtr, arguments: Arguments): TaggedValue =
  let list = untag_reference(arg(0), ListValue)
  let function = untag_reference(arg(1), FunctionValue)
  for element in list.elements:
    discard evaluator.evaluate_function_value(function, frame, element)
  values.unit_value_tagged

proc symbol_name(frame: FramePtr, arguments: Arguments): TaggedValue =
  let symbol = untag_reference(arg(0), SymbolValue)
  new_string_value_tagged(symbol.name)

proc io_println(frame: FramePtr, arguments: Arguments): TaggedValue =
  echo arg(0)
  values.unit_value_tagged

proc intr(name: string, function: IntrinsicFunction, arity: int): Intrinsic {.inline.} =
  Intrinsic(name: name, function: function, arity: arity)

let intrinsics*: seq[Intrinsic] = @[
  intr("lore.core.equal?", core_equal, 2),
  intr("lore.core.less_than?", core_less_than, 2),
  intr("lore.core.to_string", core_to_string, 1),
  intr("lore.core.panic", core_panic, 1),

  intr("lore.int.to_real", int_to_real, 1),

  intr("lore.real.to_int", real_to_int, 1),
  intr("lore.real.parse", real_parse, 1),
  intr("lore.real.nan?", real_is_nan, 1),

  #binary("lore.string.at", string_at),
  intr("lore.string.length", string_length, 1),

  intr("lore.list.each", list_each, 2),

  intr("lore.symbol.name", symbol_name, 1),

  intr("lore.io.println", io_println, 1),
]
