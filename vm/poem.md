# Poem Bytecode Format

This document describes the structure of the `.poem` bytecode format. Poem files are always written and read in big endian.

### Poems

A **Poem** is a single bytecode unit. It contains exactly one *Constants* table, any number of *TypeDeclarations*, and any number of *Function* definitions. All Functions in a Poem have the same Constants table.

Concretely, a Poem file has the following structure:

  - **Magic bytes** (char * 4): Always the string `poem` encoded in ASCII.
  - **Constants** (Constants)
  - **Type declaration count** (uint16)
  - **Type declarations** (TypeDeclaration*)
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

### Schemas

A **Schema** describes a user-defined *trait* or *struct* with optional type parameters.

TODO

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
  - TODO (vm): Type parameters.
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

### Types

A **Type** is a particular instance of a type such as `Int`, `Real | Boolean`, or `(Position, Range)`. It has the following structure:

  - **Type tag** (uint8): The kind of type and, possibly, metadata about its operands.
    - **Kind** (bits 0-2):
      - 000: Basic type (Any, Nothing, Int, Real, Boolean, String)
      - 001: Fixed-size type (Function, List, Map, Symbol)
      - 010: Sum
      - 011: Intersection
      - 100: Tuple
      - 101: Shape
      - 110: Named
    - **Metadata** (bits 3-7):
      - Basic type:
        - 00000: Any
        - 00001: Nothing
        - 00010: Int
        - 00011: Real
        - 00100: Boolean
        - 00101: String
      - Fixed-size type:
        - 00000: Function
        - 00001: List
        - 00010: Map
        - 00011: Symbol
      - Sum/Intersection/Tuple/Named: the number of operands (0 to 31)
      - Shape: the number of properties (0 to 31)
  - **Operands**:
    - Basic types:
      - *None.*
    - Function:
      - **Input type** (Type)
      - **Output type** (Type)
    - List:
      - **Element** (Type)
    - Map:
      - **Key** (Type)
      - **Value** (Type)
    - Symbol:
      - **Name** (String)
    - Sum/Intersection/Tuple:
      - **Parts/elements** (Type*)
    - Named:
      - **Name** (String)
      - **Type arguments** (Type*)
    - Shape:
      - **Properties** (ShapeProperty*):
        - **Name** (String)
        - **Type** (Type)

Note that **type variables** are represented as *Named* types and must be previously declared in a function definition or type declaration.

### Type Variables

**TypeVariables** are declarations of type variables and have the following structure:
 
  - **Name** (String)
  - **Lower bound** (Type)
  - **Upper bound** (Type)

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
