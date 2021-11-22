## TODO

#### Features

- Figure out whether `seq` and `string` deep copying by default is a significant performance problem. For example, when we call `values.new_tuple`, how many times is a sequence deep-copied before it finds its place inside `elements`? This might be a big reason why value and type creation performance is suboptimal.
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
- Add a rudimentary form of instruction validation before execution, such as that no register indices are out of bounds, and possibly simple type checking for constants.
