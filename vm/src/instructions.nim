type
  Operation* {.pure.} = enum
    ## To describe an operation, you can use the following terms:
    ##
    ##  - `argx`: The xth uint16 argument of the instruction (starting with 0).
    ##  - `reg(x)`: Register x.
    ##  - `opl(x)`: The xth entry in the operand list.
    ##  - `tpe(x)`: The xth entry in the types constants table.
    ##  - `val(x)`: The xth entry in the values constants table.
    ##  - `nam(x)`: The xth entry in the names constants table.
    ##  - `intr(x)`: The xth entry in the intrinsics constants table.
    ##  - `sch(x)`: The xth entry in the schemas constants table.
    ##  - `glb(x)`: The xth entry in the global variables constants table.
    ##  - `mf(x)`: The xth entry in the multi functions constants table.
    ##  - `mtsh(x)`: The xth entry in the meta shapes constants table.
    ##  - `targ(x)`: The xth entry in the current function instance's type arguments.
    ##  - `substitute(t)`: Substitutes the current function instance's type arguments into type `t`.
    ##  - `substitute_types(v)`: Substitutes the current function instance's type arguments into value `v`.
    ##
    ## The operand list is used by instructions which require a number of operands that exceeds the instruction's size
    ## limit. For example, to call a function with 8 arguments, we cannot pass 8 registers within the instruction,
    ## because it can only take 5 arguments (and one argument each for target and function). The operand list contains
    ## up to 256 entries and can be prepared using the `OplPushX` instructions. It is global to the evaluator and only
    ## valid for a single consumption by an instruction. The general pattern is casting `OplPushX` with the correct
    ## operand list target indices and then invoking the consuming instruction with the correct operand count. The
    ## consuming instruction must assume that the first operand is `opl(0)`.

    Const
      ## reg(arg0) <- val(arg1)

    ConstPoly
      ## reg(arg0) <- substitute_types(val(arg1))

    IntConst
      ## reg(arg0) <- arg1 as int64

    IntAdd
      ## reg(arg0) <- reg(arg1) + reg(arg2)

    IntAddConst
      ## reg(arg0) <- reg(arg1) + arg2.int

    IntSubConst
      ## reg(arg0) <- reg(arg1) - arg2.int

    IntLt
      ## reg(arg0) <- reg(arg1) < reg(arg2)

    IntLtConst
      ## reg(arg0) <- reg(arg1) < arg2.int

    IntGtConst
      ## reg(arg0) <- reg(arg1) > arg2.int

    RealAdd
      ## reg(arg0) <- reg(arg1) + reg(arg2)

    StringOf
      ## Converts any TaggedValue to its native string representation. Reference values are not converted with
      ## `lore.core.to_string`. Rather, their pointer value is printed out. This operation is a fallback for `to_string`.
      ## reg(arg0) <- string_of(reg(arg1))

    StringConcat
      ## reg(arg0) <- concat(reg(arg1), reg(arg2))

    Tuple
      ## reg(arg0) <- tuple(opl(0), ..., opl(arg1 - 1))

    Tuple0
      ## reg(arg0) <- unit

    Tuple1
      ## reg(arg0) <- tuple(reg(arg1))

    Tuple2
      ## reg(arg0) <- tuple(reg(arg1), reg(arg2))

    TupleGet
      ## reg(arg0) <- reg(arg1).get(arg2)

    FunctionCall
      ## reg(arg0) <- reg(arg1)(opl(0), ..., opl(arg2 - 1))

    FunctionCall0
      ## reg(arg0) <- reg(arg1)()

    FunctionCall1
      ## reg(arg0) <- reg(arg1)(reg(arg2))

    FunctionCall2
      ## reg(arg0) <- reg(arg1)(reg(arg2), reg(arg3))

    ListAppend
      ## reg(arg0) <- reg(arg1) :+ reg(arg2), with type `tpe(arg3)`

    ListAppendPoly
      ## reg(arg0) <- reg(arg1) :+ reg(arg2), with type `substitute(tpe(arg3))`

    ListAppendUntyped
      ## reg(arg0) <- reg(arg1) :+ reg(arg2), with type of `reg(arg1)`

    Shape
      ## reg(arg0) <- shape(mtsh(arg1), opl(0), ..., opl(arg2 - 1))

    Shape0
      ## reg(arg0) <- empty_shape

    Shape1
      ## reg(arg0) <- shape(mtsh(arg1), reg(arg2))

    Shape2
      ## reg(arg0) <- shape(mtsh(arg1), reg(arg2), reg(arg3))

    ShapePropertyGetNamed
      ## Returns the shape property value with the name `nam(arg2)`. This is only possible when accessing a value which
      ## is guaranteed to be a shape. Otherwise, PropertyGetNamed must be used.
      ## reg(arg0) <- reg(arg1)[nam(arg2)]

    SymbolEq
      ## reg(arg0) <- reg(arg1) == reg(arg2)

    Struct
      ## reg(arg0) <- sch(arg1)(opl(0), ..., opl(arg2 - 1))

    Struct0
      ## reg(arg0) <- sch(arg1)()

    Struct1
      ## reg(arg0) <- sch(arg1)(reg(arg2))

    Struct2
      ## reg(arg0) <- sch(arg1)(reg(arg2), reg(arg3))

    # TODO (vm): Add instructions like Struct2Poly1 for building simple polymorphic structs without the operand list.
    StructPoly
      ## arg2: type argument count
      ## arg3: value argument count
      ## reg(arg0) <- sch(arg1)[opl(0), ..., opl(arg2 - 1)](opl(arg2), ..., opl(arg2 + arg3 - 1))

    StructPropertyGet
      ## Returns the struct property value at the index `arg2`. This is only possible when accessing a struct value
      ## whose type is known at compile time. Otherwise, StructPropertyGetNamed or PropertyGetNamed must be used.
      ## reg(arg0) <- reg(arg1)[arg2]

    StructPropertyGetNamed
      ## Returns the struct property value with the name `nam(arg2)`. This is only possible when accessing a value which
      ## is guaranteed to be a struct (though it may be a trait at compile time). Otherwise, PropertyGetNamed must be
      ## used.
      ## reg(arg0) <- reg(arg1)[nam(arg2)]

    StructEq
      ## Whether the two structs are referentially equal.
      ## reg(arg0) <- reg(arg1) == reg(arg2)

    PropertyGetNamed
      ## Returns the struct or shape property value with the name `nam(arg2)`.
      ## reg(arg0) <- reg(arg1)[nam(arg2)]

    Jump
      ## pc <- arg0

    JumpIfFalse
      ## if !reg(arg1): pc <- arg0

    JumpIfTrue
      ## if reg(arg1): pc <- arg0

    Intrinsic0
      ## reg(arg0) <- intr(arg1)()

    Intrinsic1
      ## reg(arg0) <- intr(arg1)(reg(arg2))

    Intrinsic2
      ## reg(arg0) <- intr(arg1)(reg(arg2), reg(arg3))

    IntrinsicVoid0
      ## intr(arg0)()

    IntrinsicVoid1
      ## intr(arg0)(reg(arg1))

    IntrinsicVoid2
      ## intr(arg0)(reg(arg1), reg(arg2))

    IntrinsicFa1
      ## reg(arg0) <- intr(arg1)(frame, reg(arg2))

    IntrinsicFa2
      ## reg(arg0) <- intr(arg1)(frame, reg(arg2), reg(arg3))

    IntrinsicVoidFa1
      ## intr(arg0)(frame, reg(arg1))

    IntrinsicVoidFa2
      ## intr(arg0)(frame, reg(arg1), reg(arg2))

    GlobalGetEager
      ## Immediately gets the value of the global variable without checking whether it is initialized. This must only be
      ## used with eager global variables, or if it can be definitely proven that a lazy global variable must have been
      ## initialized at the point the instruction is used.
      ## reg(arg0) <- glb(arg1)

    GlobalGetLazy
      ## reg(arg0) <- glb(arg1)

    GlobalSet
      ## glb(arg0) <- reg(arg1)

    Dispatch
      ## reg(arg0) <- mf(arg1)(opl(0), ..., opl(arg2 - 1))

    Dispatch0
      ## reg(arg0) <- mf(arg1)()

    Dispatch1
      ## reg(arg0) <- mf(arg1)(reg(arg2))

    Dispatch2
      ## reg(arg0) <- mf(arg1)(reg(arg2), reg(arg3))

    Return
      ## return reg(arg0)

    ReturnUnit
      ## return unit

    Return0
      ## return reg(0)

    TypeArg
      ## reg(arg0) <- targ(reg1)

    TypeConst
      ## reg(arg0) <- tpe(reg1)

    OplPush1
      ## opl(arg0) <- reg(arg1)

    OplPush2
      ## opl(arg0) <- reg(arg1)
      ## opl(arg0 + 1) <- reg(arg2)

    OplPush3
      ## opl(arg0) <- reg(arg1)
      ## opl(arg0 + 1) <- reg(arg2)
      ## opl(arg0 + 2) <- reg(arg3)

    OplPush4
      ## opl(arg0) <- reg(arg1)
      ## ...
      ## opl(arg0 + 3) <- reg(arg4)

    OplPush5
      ## opl(arg0) <- reg(arg1)
      ## ...
      ## opl(arg0 + 4) <- reg(arg5)

    OplPush6
      ## opl(arg0) <- reg(arg1)
      ## ...
      ## opl(arg0 + 5) <- reg(arg6)

    Invalid
      ## Panics the VM due to an invalid state. This operation can stand in if an Operation is expected but not
      ## desired.

  Argument = distinct uint16

  Instruction* = object
    operation*: Operation
    arguments*: array[7, Argument]
      ## 7 arguments pad the size of Instruction to exactly 16 bytes.

# TODO (vm/schemas): This should be a compilation warning instead of a runtime assertion.
assert sizeof(Instruction) == 16

let maximum_instruction_arguments*: int = 7

proc is_jump_operation*(operation: Operation): bool =
  case operation
  of Operation.Jump, Operation.JumpIfFalse, Operation.JumpIfTrue: true
  else: false

template arg*(instruction: Instruction, index: uint16): uint16 = cast[uint16](instruction.arguments[index])
template argi*(instruction: Instruction, index: uint16): int16 = cast[int16](instruction.arguments[index])

proc set_arg*(instruction: var Instruction, index: uint16, value: uint16) {.inline.} =
  instruction.arguments[index] = Argument(value)

proc new_instruction*(operation: Operation, args: open_array[uint16]): Instruction =
  var arguments: array[7, Argument]
  for i in 0 ..< min(arguments.len, args.len):
    arguments[i] = Argument(args[i])
  Instruction(operation: operation, arguments: arguments)

proc new_instruction*(operation: Operation, arg0: uint16, rest: open_array[uint16]): Instruction = new_instruction(operation, @[arg0] & @rest)

proc new_instruction*(operation: Operation, arg0: uint16, arg1: uint16, arg2: uint16, arg3: uint16, arg4: uint16, arg5: uint16, arg6: uint16): Instruction =
  new_instruction(operation, [arg0, arg1, arg2, arg3, arg4, arg5, arg6])

proc new_instruction*(operation: Operation, arg0: uint16, arg1: uint16, arg2: uint16, arg3: uint16, arg4: uint16, arg5: uint16): Instruction =
  new_instruction(operation, arg0, arg1, arg2, arg3, arg4, arg5, 0)

proc new_instruction*(operation: Operation, arg0: uint16, arg1: uint16, arg2: uint16, arg3: uint16, arg4: uint16): Instruction =
  new_instruction(operation, arg0, arg1, arg2, arg3, arg4, 0, 0)

proc new_instruction*(operation: Operation, arg0: uint16, arg1: uint16, arg2: uint16, arg3: uint16): Instruction =
  new_instruction(operation, arg0, arg1, arg2, arg3, 0, 0, 0)

proc new_instruction*(operation: Operation, arg0: uint16, arg1: uint16, arg2: uint16): Instruction =
  new_instruction(operation, arg0, arg1, arg2, 0, 0, 0, 0)

proc new_instruction*(operation: Operation, arg0: uint16, arg1: uint16): Instruction =
  new_instruction(operation, arg0, arg1, 0, 0, 0, 0, 0)

proc new_instruction*(operation: Operation, arg0: uint16): Instruction =
  new_instruction(operation, arg0, 0, 0, 0, 0, 0, 0)

proc new_instruction*(operation: Operation): Instruction =
  new_instruction(operation, 0, 0, 0, 0, 0, 0, 0)
