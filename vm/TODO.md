## TODO

#### Features

- Implement all kinds of values except maps, shapes, traits, and structs:
  - Tuples: ~~Values~~, ~~to string~~, ~~poem reading~~, ~~poem writing~~, ~~universe resolution~~, ~~operations~~, ~~test example~~.
  - Symbols: ~~Values~~, ~~to string~~, ~~poem reading~~, ~~poem writing~~, ~~universe resolution~~, ~~operations~~, test example.
  - Lists: Values, to string, poem reading, poem writing, universe resolution, operations, test example.
  - Functions: Values, to string, poem reading, poem writing, universe resolution, call operation, multi-function values, fixed-function values, test example.
- Implement list append as an operation.
  - The operation must use a type evaluated at compile time. This is the reason why we're implementing the appends as an operation instead of an intrinsic (for now).
  - Implement a ListAppendUntyped operation as a faster alternative to ListAppend.
- Implement global variables: Global variable definition (eager or lazy), poem reading, poem writing, universe resolution, referencing via constants table, getter and setter operations.
- Implement intrinsics: Intrinsics definition, constants table intrinsics, universe resolution, intrinsics calls.
  - Should we allow passing type arguments to intrinsics? This would allow us to implement the list append operation as an intrinsic.
- Support lambdas directly or leave it to the compiler?
- Implement parametric functions:
  - Type parameters: poem reading, poem writing, universe resolution, type variable resolution from function scopes, run-time assignments map.
  - Types: fit, type substitution, sum/intersection simplification.
  - We probably don't need to implement the functions from `polymorphy.ts`, because all functions will have a full list of their type variable declarations. Hence, we can always find out the variables and whether a function is polymorphic without looking at the input type.
- Implement shapes:
  - Shape types: Type definition, type equality, subtyping, type variable allocation, type substitution, to string, poem reading, poem writing, universe resolution.
  - Shape values: Value definition, to string, poem reading, poem writing, universe resolution, indirect field access.
- Implement traits and structs:
  - Schemas: Schema definition, poem reading, poem writing, universe resolution.
  - Types: Type definition, type equality, subtyping, type variable allocations, type substitution, to string, poem reading, poem writing, supertrait instantiation, open property types and `getPropertyType`, constructors, open type parameters and type paths.
  - Values: Struct values, struct memory layout, poem reading, poem writing, constructors, direct and indirect field access, constructor functions, interned objects.
- Implement declared type interning:
  - Implement hashing for all types. 
  - Cache declared types within a schema.
- Implement symbol type/value interning.
  - Symbols can be represented by an integer into a symbol table that is resolved with the universe. We can use one of the unused tag bit patterns to avoid any allocations. 
- Implement a dispatch cache.
- Implement fast persistent lists.
- Implement maps: Map values, append for maps.
- Implement fast persistent maps.
- Support type introspection.
- Clear all `TODO (vm)` entries.
