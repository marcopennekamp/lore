## TODO

#### Features

- Implement shapes:
  - Shape types: ~~Type definition~~, ~~type equality~~, ~~subtyping~~, ~~type variable allocation~~, ~~type substitution~~, ~~type simplification~~, ~~to string~~, ~~poem reading~~, ~~poem writing~~, ~~universe resolution~~.
  - Shape values: Value definition, to string, poem reading, poem writing, universe resolution, value construction operations, indirect field access.
- Implement traits and structs:
  - Schemas: Schema definition, poem reading, poem writing, universe resolution.
  - Types: Type definition, type equality, subtyping, type variable allocations, type substitution, type simplification, to string, poem reading, poem writing, supertrait instantiation, open property types and `getPropertyType`, constructors, open type parameters and type paths.
  - Values: Struct values, struct memory layout, poem reading, poem writing, constructors, direct and indirect field access, constructor functions, interned objects.
- Figure out how to pass a surrounding function's type arguments to lambdas created within the function.
- Implement declared type interning:
  - Implement hashing for all types. 
  - Cache declared types within a schema.
- Implement symbol type/value interning.
  - Symbols can be represented by an integer into a symbol table that is resolved with the universe. We can use one of the unused tag bit patterns to avoid any allocations.
- Implement a dispatch cache.
- Implement fast persistent lists.
- Implement maps: Map values, append for maps.
- Implement fast persistent maps.
- `fit` performance improvement: Can't we combine the `is_subtype` check with the type variable assignments phase? This might at least save some allocations from the substitution.
  - An alternative idea might be to implement an `is_subtype` proc specifically for the fit which uses the type variable assignments instead of substituting these assignments into a newly allocated type. This would keep the phases separate, but still save all allocations.
  - In general, `fit` should only make a single allocation of an ImSeq in case the fit is successful. All other allocations should technically be avoidable, with type variable assignments initially placed on the stack.
- Support type introspection.
- Clear all `TODO (vm)` entries.
- Add a rudimentary form of instruction validation before execution, such as that no register indices are out of bounds, and possibly simple type checking for constants.
