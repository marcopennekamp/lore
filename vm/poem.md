# Poem Bytecode Format

This document describes the structure of the `.poem` bytecode format. Poem files are always written and read in big endian.

### Poems

A **Poem** is a single bytecode unit. It contains exactly one *Constants* table, any number of *TypeDeclarations*, and any number of *Function* definitions. All Functions in a Poem have the same Constants table.

Concretely, a Poem file has the following structure:

  - **Magic bytes** (char * 4): Always the string `poem` encoded in ASCII.
  - **Constants** (Constants)
  - **Type declaration count** (uint16)
  - **Type declarations** (TypeDeclaration*)
  - **Meta shape count** (uint16)
  - **Meta shapes** (MetaShape*)
  - **Global variable count** (uint16)
  - **Global variables** (GlobalVariable*)
  - **Function count** (uint16)
  - **Functions** (Function*)

### Constants

The **Constants** table holds Poem-wide constant types and values, as well as references to intrinsics, global variables, and multi-functions. Each kind of constant is indexed separately by a 16-bit unsigned integer.

The Constants table has the following structure:

  - **Types count** (uint16)
  - **Types** (Type*)
  - **Values count** (uint16)
  - **Values** (Value*)
  - **Intrinsics count** (uint16)
  - **Intrinsics** (String*): The full names of the intrinsic references.
  - **Global variables count** (uint16)
  - **Global variables** (String*): The full names of the global variable references.
  - **Multi-functions count** (uint16)
  - **Multi-functions** (String*): The full names of the multi-function references.

### Meta Shapes

A **MetaShape** describes the property names that a shape type may have:

- **Property name count** (uint8)
- **Property names** (String*)

The property names must be ordered lexicographically and may not contain duplicates. A shape type and shape value must contain their property types/values in the order defined by the meta shape. The property name count of the meta shape directly determines the number of property types/values read for a shape type/value.

### Global Variables

A **GlobalVariable** is represented as follows:

  - **Name** (String)
  - **Lazy** (bool)
  - If *Lazy* is false:
    - **Value** (Value)
  - If *Lazy* is true:
    - **Initializer Name** (String): The name of the function with which the lazy global variable should be initialized when it's first accessed. The function name must refer to a multi-function with exactly one function definition.

Note that global variables are currently *not* typed. It would only be needed for checking bytecode, because values carry their own types.

### Functions

A **Function** represents a single function definition. Its structure is as follows:

  - **Name** (String)
  - **Type parameter count** (uint8): Maximum 32.
  - **Type parameters** (TypeParameter*)
  - **Input type** (TupleType)
  - **Output type** (Type)
  - **Abstract** (bool)
  - If *Abstract* is false:
    - **Register count** (uint16)
    - **Instruction count** (uint16)
    - **Instructions** (Instruction*)

### Instructions

Instructions are encoded as fixed-size instructions:

  - **Operation** (uint16)
  - **Arguments** (uint16 * 3)

### Schemas

A **Schema** describes a user-defined *trait* or *struct* with optional type parameters.

TODO

### Type Parameters

**TypeParameter** declarations have the following structure:

- **Name** (String)
- **Lower bound** (Type)
- **Upper bound** (Type)
- **Variance** (uint8):
  - Covariant
  - Contravariant
  - Invariant

### Types

A **Type** is a particular instance of a type such as `Int`, `Real | Boolean`, or `(Position, Range)`. It has the following structure:

  - **Type tag** (uint8): The kind of type and, possibly, information about its operand count.
    - **Kind** (bits 0-2):
      - 000: Metadata-kinded type (Any, Nothing, Int, Real, Boolean, String, Variable, Function, List, Map, Shape, Symbol)
      - 001: Sum
      - 010: Intersection
      - 011: Tuple
      - 100: Named
    - **Metadata** (bits 3-7):
      - Metadata-kinded type:
        - 00000: Any
        - 00001: Nothing
        - 00010: Int
        - 00011: Real
        - 00100: Boolean
        - 00101: String
        - 10000: Variable
        - 10001: Function
        - 10010: List
        - 10011: Map
        - 10100: Shape
        - 10101: Symbol
      - Sum/Intersection/Tuple/Named: the number of operands (0 to 31)
  - **Operands**:
    - Any/Nothing/Int/Real/Boolean/String:
      - *None.*
    - Variable:
      - **Index** (uint8)
    - Function:
      - **Input type** (Type)
      - **Output type** (Type)
    - List:
      - **Element** (Type)
    - Map:
      - **Key** (Type)
      - **Value** (Type)
    - Shape:
      - **Meta shape index** (uint16): An index into the poem's meta shape declarations, referring to the shape type's meta shape.
      - **Property types count** (uint8): This count could be derived from the meta shape, but it would introduce data dependencies during reading which we want to avoid.
      - **Property types** (Type*): The property types must be ordered in the manner prescribed by the meta shape.
    - Symbol:
      - **Name** (String)
    - Sum/Intersection/Tuple:
      - **Parts/elements** (Type*)
    - Named:
      - **Name** (String)
      - **Type arguments** (Type*)

### Values

A **Value** is encoded as follows:

  - **Type** (Type)
  - **Representation** (varying): Depending on the value's type:
    - Int:
      - **Value** (int64)
    - Real:
      - **Value** (float64)
    - Boolean:
      - **Value** (bool)
    - String:
      - **Value** (String)
    - Tuple:
      - **Elements** (Value*)
      - The element count is given by *Type*.
    - Function:
      - **Variant** (uint8):
        - Fixed: A fixed function with a fixed input type resolved during universe resolution.
        - Lambda: A lambda function. The VM's bytecode encodes lambda functions as single-function multi-functions. We could technically access lambdas as fixed functions, but this variant allows the compiler to omit the fixed input type from the bytecode, saving space.
        - Multi: A multi-function value.
      - **Name** (string): The name of the targeted multi-function.
      - If `Fixed`:
        - **Input type** (Type): The desired input type that the fixed function should match.
    - List:
      - **Element count** (uint16) 
      - **Elements** (Value*)
    - Symbol:
      - *Type* already carries the name of the symbol value.
    - TODO (vm): Describe value encodings for more value kinds.

### Strings

A UTF-8 **String** is encoded as follows:

  - **Size** (uint16): The string's size in bytes.
  - **Characters** (uint8 * Size)
