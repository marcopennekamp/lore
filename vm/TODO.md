## TODO

#### Features

- Implement traits and structs:
  - General:
    - Types: ~~supertrait instantiation~~, `find_supertrait`.
  - Traits: 
    - Types: ~~Schema definition~~, ~~type definition~~, ~~poem reading~~, ~~poem writing~~, ~~universe resolution~~, type equality, subtyping, type variable allocation, type substitution, type simplification, to string.
  - Structs:
    - Types: ~~Schema definition~~, ~~type definition~~, poem reading, poem writing, universe resolution, type equality, subtyping, type variable allocation, type substitution, type simplification, to string, open property types and `get_property_type`, constructor, open type parameters and type paths.
    - Values: Struct values, poem reading, poem writing, direct and indirect property access, constructor, constructor functions.
      - Objects can be represented by a (lazy?) global variable that contains the sole instance of the struct. This can all be checked and implemented by the compiler, so the VM might not even need to differentiate between objects and normal structs.
  - Clear all `TODO (vm/schemas)` entries.
- Figure out how to pass a surrounding function's type arguments to lambdas created within the function.
- Implement declared type interning:
  - Implement hashing for all types.
    - Clear all `TODO (vm/hash)` entries.
  - Cache declared types within a schema.
  - Clear all `TODO (vm/intern)` entries.
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
- Support parallel execution.
  - Clear all `TODO (vm/parallel)` entries.
- Clear all `TODO (vm)` entries.
- Add a rudimentary form of instruction validation before execution, such as that no register indices are out of bounds, and possibly simple type checking for constants.
- We can flatten many ImSeqs into UncheckedArrays or ImSeqObj for types and values (especially tuples, sum/intersection types, etc.). This will save a lot of allocations when creating values, but also make the VM's code a bit more complicated. We'll either have to pass around these embedded ImSeqObj as pointers, or write some glue code here and there.
