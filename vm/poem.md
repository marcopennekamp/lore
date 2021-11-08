# Poem Bytecode Format

This document describes the structure of the `.poem` bytecode format. Poem files are always written and read in big endian.

### Poems

A **Poem** is a single bytecode unit. It contains exactly one *Constants* table, any number of *TypeDeclarations*, and any number of *Function* definitions. All Functions in a Poem have the same Constants table.

Concretely, a Poem file has the following structure:

  - **Magic bytes** (char * 4): Always the string `poem` encoded in ASCII.
  - **Constants** (Constants)
  - **Type declaration count** (uint16)
  - **Type declarations** (Type declaration)
  - **Function count** (uint16)
  - **Functions** (Function)

### Constants

The **Constants** table holds Poem-wide constant types, multi-functions, and values. Each kind of constant is indexed separately by a 16-bit unsigned integer.

The Constants table has the following structure:

  - **Types count** (uint16): The number of type constants.
  - **Types** (Type*): The type constants.
  - **Values count** (uint16): The number of value constants.
  - **Values** (Value*): The value constants.
  - **Multi-functions count** (uint16): The number of multi-function constants.
  - **Multi-functions** (String*): The full names of multi-function constants.

### Schemas

A **Schema** describes a user-defined *trait* or *struct* with optional type parameters.

TODO

### Functions

A **Function** represents a single function definition. Its structure is as follows:

  - **Name** (String)
  - TODO (vm): Type parameters.
  - **Input type** (TupleType)
  - **Output type** (Type)
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
      - **Value** (uint8)
    - String:
      - **Value** (String)
    - Tuple:
      - **Elements** (Value*)
      - The element count is given by *Type*.
    - TODO (vm): Describe value encodings for more value kinds.

### Strings

A UTF-8 **String** is encoded as follows:

  - **Size** (uint16): The string's size in bytes.
  - **Characters** (uint8 * Size)
