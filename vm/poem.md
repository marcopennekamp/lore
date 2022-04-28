# Poem Bytecode Format

This document describes the structure of the `.poem` bytecode format. Poem files are always written and read in big endian.

### Poems

A **Poem** is a single bytecode unit. It contains any number of *Schemas*, *GlobalVariables*, and *Functions*. 

A VM program may consist of many poems. An explicit organisation of which items to put into which poem files is not prescribed. The Lore compiler usually puts a whole program into a single poem file, but it's also possible to put each function and type definition into its own poem (though not recommended), or anything in between. The VM is designed to handle fragmented multi-function definitions across as many poem files as necessary.

Concretely, a Poem file has the following structure:

  - **Magic bytes** (char * 4): Always the string `poem` encoded in ASCII.
  - **Schema count** (uint32)
  - **Schemas** (Schema*)
  - **Global variable count** (uint32)
  - **Global variables** (GlobalVariable*)
  - **Function count** (uint32)
  - **Functions** (Function*)

### Schemas

A **Schema** describes a user-defined *trait* or *struct* with optional type parameters. It's represented as follows:

  - **Kind** (uint8):
    - 0: Trait
    - 1: Struct
  - **Name** (String)
  - **Type parameter count** (uint8): Maximum 32.
  - **Type parameters** (TypeParameter*)
  - **Supertrait count** (uint8)
  - **Supertraits** (NamedType*): The schema's directly extended supertraits.
  - If `Trait`:
    - **Inherited shape type** (ShapeType)
  - If `Struct`:
    - **Property count** (uint16)
    - **Properties** (StructProperty*): Struct properties must be ordered lexicographically by their name.

Any types inside any of the schema's fields may contain type variables that refer to the schema's type parameters.

Some of Lore's struct features aren't directly implemented by the VM:

  - **Constructors** are just parameterized functions. The construction operations for structs handle open property types due to their importance to the type system. **Open type parameters** and **default values**, on the other hand, have to be implemented by adding the right bytecode instructions to the constructor.
  - **Constructor functions** are just properly instantiated function values.
  - An **object** can be represented by a global variable that contains the only instance of the struct. This can all be checked and implemented by the compiler, so the VM does not need to differentiate between objects and normal structs.

### Struct Property

A **StructProperty** describes a Schema struct property:

  - **Name** (String)
  - **Type** (Type): The property's type may contain type variables, which will reference the struct's type parameters.
  - **Open** (bool): Whether the property is open.
  - **Declaration index** (uint16): The index of the property in its original declaration order.

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
    - **Constants** (Constants)
    - **Register count** (uint16)
    - **Instruction count** (uint16)
    - **Instructions** (Instruction*)

Note that functions backing lambdas (used with the `FunctionLambda` instruction) may not have type parameter *bounds*. The VM expects all type parameters to have the bounds `Nothing` and `Any`.

### Function Instances

A poem **FunctionInstance** represents the constant instantiation of a single-function multi-function. It has the following structure:

- **Name** (String)
- **Type argument count** (uint8)
- **Type arguments** (Type*)

### Constants

The **Constants** table holds Function-wide constant types, values, and names, as well as references to intrinsics, schemas, global variables, multi-functions, function instances, and meta shapes. Constants are indexed by a 16-bit unsigned integer ID global to the constants table. For example, if the constants table has two types and two values, the ID of the second value would be 3. Constants do not need to be ordered by their variant. For example, a constants table may consist of a value, schema, type, function instance, and another type, in this order.

The constants table has the following structure:

- **Entry count** (uint16)
- **Entries** (ConstantsEntry*)

### ConstantsEntry

A **ConstantsEntry** has the following structure:

- **Variant** (uint8):
  - 0: Type
  - 1: Value
  - 2: Name
  - 3: Intrinsic
  - 4: Schema
  - 5: GlobalVariable
  - 6: MultiFunction
  - 7: FunctionInstance
  - 8: MetaShape
- The representation depends on *Variant*:
  - Type:
    - **Type** (Type)
  - Value:
    - **Value** (Value)
  - Name, Intrinsic, Schema, GlobalVariable, MultiFunction:
    - **Name** (String): The name of the intrinsic/schema/global variable/multi-function.
  - FunctionInstance:
    - **FunctionInstance** (FunctionInstance): Function instance constants are used by `Call` instructions to directly call a function instance, without the need to create a function instance or a constant function value.
  - MetaShape:
    - **MetaShape** (MetaShape): These meta shapes are exclusively used by instructions creating new shape instances. They are not referenced by constant types or values.

### Instructions

**Instructions** are encoded as variable size depending on the instruction. They correspond heavily to the actual instructions evaluated by the VM. The poem API makes some simplifications, such as providing a single dispatch operation instead of `Dispatch`, `Dispatch0`, `Dispatch1`, `Dispatch2`, and so on. A poem instruction may spawn multiple evaluator instructions, making it possible to hide operand lists behind the API.

Variable-size instructions have two big advantages compared to fixed-size instructions:

  - They reduce the size of uncompressed binaries.
  - They hide implementation-specific details of the evaluation, such as operands lists and frame-aware intrinsics.

TODO (vm): Document instruction encoding in-depth.

### Meta Shapes

A **MetaShape** describes the property names that a shape may have:

- **Property name count** (uint8)
- **Property names** (String*)

The property names must be ordered lexicographically and may not contain duplicates. Meta shapes for constant types and values are defined ad-hoc, to avoid complex dependencies within poems. This MetaShape structure is placed in the constants table and used by instructions that build shape values.

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
      - **Property count** (uint8)
      - **Property names** (String*): The property names must be ordered lexicographically and may not contain duplicates.
      - **Property types** (Type*): The property types must be in the same order as the property names.
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
        - Multi: A multi-function value.
        - Single: A single function is backed by a single-function multi-function, with its type arguments specified. 
          - Single function values can be used to represent lambdas as constant values, but only those that are created inside monomorphic functions and capture no variables. All other lambdas must be created at run-time using the `Lamdba` operation.
        - Fixed: A fixed function with a fixed input type resolved during universe resolution.
      - **Name** (string): The name of the targeted multi-function.
      - If `Single`:
        - **Type argument count** (uint8) 
        - **Type arguments** (Type*) 
      - If `Fixed`:
        - **Input type** (Type): The desired input type that the fixed function should match.
    - List:
      - **Element count** (uint16) 
      - **Elements** (Value*)
    - Shape:
      - **Property values** (Value*): The property values must be in the same order as the type's property names.
    - Symbol:
      - *Type* already carries the name of the symbol value.
    - Struct:
      - **Property count** (uint16)
      - **Property values** (Value*): The property values must be in the same order as the struct schema's property names, which is implicitly determined by the value's NamedType.

### Strings

A UTF-8 **String** is encoded as follows:

  - **Size** (uint16): The string's size in bytes.
  - **Characters** (uint8 * Size)
