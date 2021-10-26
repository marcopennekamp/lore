from types import Kind, Type

type
  Value* = ref object of RootObj
    tpe*: Type  # TODO (vm): How can we make this an IntType for IntValues? Maybe the variant objects aren't a good idea after all?

  IntValue* = ref object of Value
    value*: int

proc new_int*(value: int): IntValue = IntValue(value: value, tpe: types.int)
