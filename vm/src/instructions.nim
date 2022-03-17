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
    ##  - `fin(x)`: The xth entry in the function instances constants table.
    ##  - `mtsh(x)`: The xth entry in the meta shapes constants table.
    ##  - `targ(x)`: The xth entry in the current function instance's type arguments.
    ##  - `lctx(x)`: The xth value in the lambda context.
    ##  - `substitute(t)`: Substitutes the current function instance's type arguments into type `t`.
    ##
    ## The operand list is used by instructions which require a number of operands that exceeds the instruction's size
    ## limit. For example, to call a function with 8 arguments, we cannot pass 8 registers within the instruction,
    ## because it can only take 5 arguments (and one argument each for target and function). The operand list contains
    ## up to 256 entries and can be prepared using the `OplPushX` instructions. It is global to the evaluator and only
    ## valid for a single consumption by an instruction. The general pattern is casting `OplPushX` with the correct
    ## operand list target indices and then invoking the consuming instruction with the correct operand count. The
    ## consuming instruction must assume that the first operand is `opl(0)`.
    ##
    ## There are further terms for convenience:
    ##
    ##  - `opl(a .. b)`: All operands in the operand list from `a` to `b`, exclusive.
    ##  - `arg(a .. b)`: All instruction arguments from `a` to `b`, exclusive.
    ##  - `reg_arg(x)` is the same as `reg(arg(x))`.

    Assign
      ## reg(arg0) <- reg(arg1)

    Const
      ## reg(arg0) <- val(arg1)

    IntConst
      ## reg(arg0) <- arg1 as int64

    IntNeg
      ## reg(arg0) <- -reg(arg1)

    IntAdd
      ## reg(arg0) <- reg(arg1) + reg(arg2)

    IntSub
      ## reg(arg0) <- reg(arg1) - reg(arg2)

    IntMul
      ## reg(arg0) <- reg(arg1) * reg(arg2)

    IntDiv
      ## reg(arg0) <- reg(arg1) / reg(arg2)

    IntEq
      ## reg(arg0) <- reg(arg1) == reg(arg2)

    IntLt
      ## reg(arg0) <- reg(arg1) < reg(arg2)

    IntLte
      ## reg(arg0) <- reg(arg1) <= reg(arg2)

    IntToReal
      ## reg(arg0) <- reg(arg1) as Real

    RealNeg
      ## reg(arg0) <- -reg(arg1)

    RealAdd
      ## reg(arg0) <- reg(arg1) + reg(arg2)

    RealSub
      ## reg(arg0) <- reg(arg1) - reg(arg2)

    RealMul
      ## reg(arg0) <- reg(arg1) * reg(arg2)

    RealDiv
      ## reg(arg0) <- reg(arg1) / reg(arg2)

    RealEq
      ## reg(arg0) <- reg(arg1) == reg(arg2)

    RealLt
      ## reg(arg0) <- reg(arg1) < reg(arg2)

    RealLte
      ## reg(arg0) <- reg(arg1) <= reg(arg2)

    BooleanConst
      ## reg(arg0) <- arg1.boolean

    BooleanNot
      ## reg(arg0) <- not reg(arg1)

    BooleanOr
      ## reg(arg0) <- reg(arg1) or reg(arg2)

    BooleanAnd
      ## reg(arg0) <- reg(arg1) and reg(arg2)

    StringOf
      ## reg(arg0) <- string_of(reg(arg1))
      ##
      ## Converts any TaggedValue to its native string representation. Values are not converted with
      ## `lore.core.to_string`, but rather the VM's native stringification capabilities. Hence, this operation should
      ## be used to cheaply stringify primitives.

    StringConcat
      ## reg(arg0) <- concat(reg(arg1), reg(arg2))

    StringEq
      ## reg(arg0) <- reg(arg1) == reg(arg2)

    StringLt
      ## reg(arg0) <- reg(arg1) < reg(arg2)
      ##
      ## Compares two strings lexicographically.

    StringLte
      ## reg(arg0) <- reg(arg1) <= reg(arg2)
      ##
      ## Compares two strings lexicographically.

    Tuple
      ## reg(arg0) <- tuple(opl(0 .. arg1))

    Tuple0
      ## reg(arg0) <- unit

    Tuple1
      ## reg(arg0) <- tuple(reg(arg1))

    Tuple2
      ## reg(arg0) <- tuple(reg(arg1), reg(arg2))

    TupleGet
      ## reg(arg0) <- reg(arg1)[arg2]

    FunctionCall
      ## reg(arg0) <- reg(arg1)(opl(0 .. arg2))

    FunctionCall0
      ## reg(arg0) <- reg(arg1)()

    FunctionCall1
      ## reg(arg0) <- reg(arg1)(reg(arg2))

    FunctionCall2
      ## reg(arg0) <- reg(arg1)(reg(arg2), reg(arg3))

    FunctionSingle
      ## reg(arg0) <- mf(arg1).instantiate_single_function(opl(0 .. arg2))
      ##
      ## Creates a new single function value from the given single-function multi-function and the type arguments from
      ## the operand list.
      ##
      ## `FunctionSingle` will be used rarely. It's only needed when the type arguments contain type variables (a value
      ## constant can be used in other cases), so there are no optimized instructions such as `FunctionSingle1`.

    Lambda
      ## reg(arg0) <- lambda(mf(arg1), targ, opl(0 .. arg3)), with type `tpe(arg2)`
      ##
      ## Creates a new lambda function value with `targ` as type arguments and the given registers as captured values.
      ## The multi-function the lambda is derived from must be a single function. The function must have the same
      ## number of type parameters, which must be unbounded.

    Lambda0
      ## reg(arg0) <- lambda(mf(arg1), targ), with type `tpe(arg2)`

    LambdaPoly
      ## reg(arg0) <- lambda(mf(arg1), targ, opl(0 .. arg3)), with type `substitute(tpe(arg2))`

    LambdaPoly0
      ## reg(arg0) <- lambda(mf(arg1), targ), with type `substitute(tpe(arg2))`

    LambdaLocal
      ## reg(arg0) <- lctx(arg1)

    List
      ## reg(arg0) <- list(opl(0 .. arg2)), with type `tpe(arg1)`

    List0
      ## reg(arg0) <- empty_list, with type `tpe(arg1)`

    List1
      ## reg(arg0) <- list(reg(arg2)), with type `tpe(arg1)`

    ListPoly
      ## reg(arg0) <- list(opl(0 .. arg2)), with type `substitute(tpe(arg1))`

    ListPoly0
      ## reg(arg0) <- empty_list, with type `substitute(tpe(arg1))`

    ListPoly1
      ## reg(arg0) <- list(reg(arg2)), with type `substitute(tpe(arg1))`

    ListAppend
      ## reg(arg0) <- reg(arg1) :+ reg(arg2), with type `tpe(arg3)`

    ListAppendPoly
      ## reg(arg0) <- reg(arg1) :+ reg(arg2), with type `substitute(tpe(arg3))`

    ListAppendUntyped
      ## reg(arg0) <- reg(arg1) :+ reg(arg2), with type of `reg(arg1)`

    ListLength
      ## reg(arg0) <- reg(arg1).length

    ListGet
      ## reg(arg0) <- reg(arg1)[reg(arg2)]

    Shape
      ## reg(arg0) <- shape(mtsh(arg1), opl(0 .. arg2))

    Shape0
      ## reg(arg0) <- empty_shape

    Shape1
      ## reg(arg0) <- shape(mtsh(arg1), reg(arg2))

    Shape2
      ## reg(arg0) <- shape(mtsh(arg1), reg(arg2), reg(arg3))

    ShapePropertyGetNamed
      ## reg(arg0) <- reg(arg1)[nam(arg2)]
      ##
      ## Returns the shape property value with the name `nam(arg2)`. This is only possible when accessing a value which
      ## is guaranteed to be a shape. Otherwise, PropertyGetNamed must be used.

    SymbolEq
      ## reg(arg0) <- reg(arg1) == reg(arg2)

    Struct
      ## arg2: value argument count
      ## reg(arg0) <- tpe(arg1).new_struct(opl(0 .. arg2))

    StructDirect
      ## arg2: value argument count
      ## reg(arg0) <- tpe(arg1).new_struct(reg_arg(3 .. 3 + arg2))

    StructPoly
      ## arg2: type argument count
      ## arg3: value argument count
      ## reg(arg0) <- sch(arg1)[opl(0 .. arg2)](opl(arg2 .. arg2 + arg3))

    StructPolyDirect
      ## arg2 (composite):
      ##  - nt: uint8 (type argument count)
      ##  - nv: uint8 (value argument count)
      ##
      ## reg(arg0) <- sch(arg1)[reg_arg(3 .. 3 + nt)](reg_arg(3 + nt, 3 + nt + nv))

    StructPropertyGet
      ## reg(arg0) <- reg(arg1)[arg2]
      ##
      ## Returns the struct property value at the index `arg2`. This is only possible when accessing a struct instance
      ## whose type is known at compile time. Otherwise, StructPropertyGetNamed or PropertyGetNamed must be used.

    StructPropertyGetNamed
      ## reg(arg0) <- reg(arg1)[nam(arg2)]
      ##
      ## Returns the struct property value with the name `nam(arg2)`. This is only possible when accessing a struct
      ## instance which is guaranteed to be a struct (though it may be a trait at compile time). Otherwise,
      ## PropertyGetNamed must be used.

    StructPropertySet
      ## reg(arg0)[arg1] <- reg(arg2)
      ##
      ## Sets the struct property at the index `arg1` to `reg(arg2)`. This is only possible when accessing a struct
      ## instance whose type is known at compile time. Otherwise, StructPropertySetNamed must be used.

    StructPropertySetNamed
      ## reg(arg0)[nam(arg1)] <- reg(arg2)
      ##
      ## Sets the struct property with the name `nam(arg2)` to `reg(arg2)`.

    StructEq
      ## reg(arg0) <- reg(arg1) == reg(arg2)
      ##
      ## Whether the two structs are referentially equal.

    PropertyGetNamed
      ## reg(arg0) <- reg(arg1)[nam(arg2)]
      ##
      ## Returns the struct or shape property value with the name `nam(arg2)`.

    Jump
      ## pc <- arg0

    JumpIfFalse
      ## if !reg(arg1): pc <- arg0

    JumpIfTrue
      ## if reg(arg1): pc <- arg0

    Intrinsic
      ## arg1: argument count
      ## reg(arg0) <- intr(arg1)(frame, [opl(0 .. arg2)])

    IntrinsicDirect
      ## arg1: argument count
      ## reg(arg0) <- intr(arg1)(frame, [reg_arg(3 .. 3 + arg2)])

    GlobalGetEager
      ## reg(arg0) <- glb(arg1)
      ##
      ## Immediately gets the value of the global variable without checking whether it is initialized. This must only
      ## be used with eager global variables, or if it can be definitely proven that a lazy global variable must have
      ## been initialized at the point the instruction is used.

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

    Call
      ## arg2: value argument count
      ## reg(arg0) <- fin(arg1)(opl(0 .. arg2))

    CallDirect
      ## arg2: value argument count
      ## reg(arg0) <- fin(arg1)(reg_arg(3 .. 3 + arg2))

    CallPoly
      ## arg2: type argument count
      ## arg3: value argument count
      ## reg(arg0) <- mf(arg1).single_function[opl(0 .. arg2)](opl(arg2 .. arg2 + arg3))

    CallPolyDirect
      ## arg2 (composite):
      ##  - nt: uint8 (type argument count)
      ##  - nv: uint8 (value argument count)
      ##
      ## reg(arg0) <- mf(arg1).single_function[reg_arg(3 .. 3 + nt)](reg_arg(3 + nt, 3 + nt + nv))

    Return
      ## return reg(arg0)

    Return0
      ## return reg(0)

    TypeArg
      ## reg(arg0) <- targ(arg1)

    TypeConst
      ## reg(arg0) <- tpe(arg1)

    TypeConstPoly
      ## reg(arg0) <- substitute(tpe(arg1))

    TypeOf
      ## reg(arg0) <- type_of(reg(arg1))

    TypePathIndex
      ## reg(arg0) <- reg(arg1)[arg2]
      ##
      ## Gets the type at index `arg2` from the tuple, function, list, or map type `reg(arg1)`. For function and map
      ## types, indices 0 and 1 refer to the input/output and key/value types, respectively.

    TypePathProperty
      ## reg(arg0) <- reg(arg1).property_types[nam(arg2)]
      ##
      ## Gets the property type named `nam(arg2)` of the shape, struct or trait type `reg(arg1)`.

    TypePathTypeArgument
      ## reg(arg0) <- find_supertype(reg(arg1), sch(arg2)).type_arguments[arg3]
      ##
      ## Gets the type argument at index `arg3` of schema `sch(arg2)` of the trait or struct type `reg(arg1)`.

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

when sizeof(Instruction) != 16:
  {.error: "Instructions must be exactly 16 bytes wide.".}

const maximum_instruction_arguments*: int = 7

proc is_jump_operation*(operation: Operation): bool =
  case operation
  of Operation.Jump, Operation.JumpIfFalse, Operation.JumpIfTrue: true
  else: false

template arg*(instruction: Instruction, index: int): uint16 = cast[uint16](instruction.arguments[index])
template arg*(instruction: Instruction, index: uint16): uint16 = cast[uint16](instruction.arguments[index])
template argi*(instruction: Instruction, index: uint16): int16 = cast[int16](instruction.arguments[index])
template argb*(instruction: Instruction, index: uint16): bool = instruction.arg(index) == 1

template argu8l*(instruction: Instruction, index: uint16): uint8 =
  ## Gets the left uint8 from a composite argument.
  cast[uint8](instruction.arg(index) shr 8)

template argu8r*(instruction: Instruction, index: uint16): uint8 =
  ## Gets the right uint8 from a composite argument.
  cast[uint8](instruction.arg(index))

template arguments_unchecked*(instruction: Instruction): ptr UncheckedArray[Argument] =
  cast[ptr UncheckedArray[Argument]](unsafe_addr instruction.arguments)

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

proc `$`*(argument: Argument): string = $uint16(argument)

proc `$`*(instruction: Instruction): string =
  let arguments = @(instruction.arguments)
  $instruction.operation & "[" & $arguments & "]"
