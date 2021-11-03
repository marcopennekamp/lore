from types import Kind, Type

type
  ## A TaggedValue is a compact representation of a Lore value. Every Lore value needs to carry type information at run
  ## time, including the primitives Int, Real, and Boolean. This is because at times, the compiler might encounter a
  ## sum type such as `Int | Real` or `Real | #error`. Such a type might stand in a list, or as a parameter, or
  ## anywhere else. Especially in storage situations (such as lists), we want to tag values as efficiently as possible
  ## without the need to create separate List implementations for each primitive.
  ##
  ## We can keep the size of TaggedValue to 8 bytes with a technique called pointer tagging. Essentially, on 64-bit
  ## systems the lower three bits of a pointer are always 0 due to alignment, so we can use these bits to tag Ints,
  ## Reals, Booleans, and references. This reduces the range of an Int, for example, from 64 bits to 61 bits, but we
  ## don't anticipate any issues with that in the foreseeable future. There are no plans to port Lore to 32-bit
  ## systems.
  ##
  ## There is a slight performance cost associated with TaggedValues, as every time we want to use an Int or a Real, we
  ## have to untag it using a shift. Every time we want to create an Int or Real, we have to shift the value and OR the
  ## tag bits. Reference access is free, as the reference tag is 0. Booleans we don't need shifts, because there are
  ## only two possible values: `0b0011` for false and `0b1011` for true, which includes the tag bits.
  TaggedValue* {.union.} = object
    uint*: uint64
    reference*: Value
    int: int64

  Value* = ref object of RootObj
    tpe*: Type

const
  ## This mask can filter out the lowest three tag bits of a pointer.
  TagMask: uint64 = 0b111
  TagReference: uint64 = 0b000
  TagInt: uint64 = 0b001
  TagReal: uint64 = 0b010  # TODO (vm): Implement Real value tagging and untagging.
  TagBoolean: uint64 = 0b011
  False: uint64 = 0 or TagBoolean
  True: uint64 = (1 shl 3) or TagBoolean

proc is_reference*(value: TaggedValue): bool = (value.uint and TagMask) == TagReference
proc is_int*(value: TaggedValue): bool = (value.uint and TagMask) == TagInt
proc is_boolean*(value: TaggedValue): bool = (value.uint and TagMask) == TagBoolean

proc tag_reference*(value: Value): TaggedValue = TaggedValue(reference: value)
proc untag_reference*(value: TaggedValue): Value = value.reference

proc tag_int*(value: int64): TaggedValue = TaggedValue(int: (value shl 3) or cast[int64](TagInt))
proc untag_int*(value: TaggedValue): int64 = value.int shr 3

proc tag_boolean*(value: bool): TaggedValue = TaggedValue(uint: if value: True else: False)
proc untag_boolean*(value: TaggedValue): bool = value.uint == True

# TODO (vm): Write a `type_of` function.
