## TODO

#### Features

- Support type introspection.
- Implement maps: Map values, append for maps.
- Support parallel execution.
  - Clear all `TODO (vm/parallel)` entries.
- Clear all `TODO (vm)` entries.

#### Correctness

- Implement a rudimentary form of instruction validation before execution, such as that no register indices are out of bounds, and possibly simple type checking for constants.

#### Optimization

- Implement declared type interning:
  - Implement hashing for all types.
    - Clear all `TODO (vm/hash)` entries.
  - Cache declared types within a schema.
  - Best intern declared types as weak references so that they can be reclaimed if no values use the types.
  - Clear all `TODO (vm/intern)` entries.
- Implement symbol type/value interning.
  - Symbols can be represented by an integer into a symbol table that is resolved with the universe. We can use one of the unused tag bit patterns to avoid any allocations.
- Implement a dispatch cache.
- Implement fast persistent lists.
- Implement fast persistent maps.
- We can flatten many `ImSeq`s into `UncheckedArray`s or `ImSeqObj`s for types and values (especially tuples, shapes, sum/intersection types, etc.). This will save allocations when creating values, but also make the VM's code a bit more complicated. We'll either have to pass around these embedded ImSeqObj as pointers, or write some glue code here and there.
- Turn declared type subtyping into a simple HashSet lookup so that we don't need to branch up the supertype tree to decide whether one declared type is the subtype of another. This would be possible by giving each type an exhaustive (transitive) list of supertypes. Downsides might become apparent especially once we introduce dynamic specialization.
  - This is probably not an optimization we want to implement as long as the language is still immature.

##### Dispatch Optimization

- Optimizing single function dispatch: We can rewrite `Dispatch` instructions that have single-function multi-functions as their target to use `Call` instructions. This is easily possible for a monomorphic function, but there are ways to treat polymorphic functions in this way too. We just have to assure that a call that is valid at compile time doesn't become invalid at run time. This means that none of the type parameters may have a lower bound, as that might exclude a subtype at run time. In addition, type parameters may not occur twice in the input type so that missing type equality cannot rule out the validity of the call. Polymorphic functions may still require assigning an argument type to a type variable, but this could then be done ad-hoc in the single function.
- If the function to be called is a leaf in the hierarchy, i.e. it isn't specialized further, we can call that function directly, because no other functions exist that could specialize the one function contained in the fit. This requires poem dispatch instructions to carry compile-time argument type information. 
  - The optimization is quite complex due to the problem mentioned below. However, it's easy to implement for calls where none of the compile-time arguments contain trait or shape types.
  - Problem: Let's say we have a concrete function `f(a: A)` with `A` being a trait. We also have a trait `Y` and a concrete function `f(y: Y)`. We have a struct `A1 extends A, Y`. The optimization above leads us to call function `f(a: A)` directly, given a value of type `A` at compile time. However, at run time, this value is actually an `A1`. Calling `f` directly with it should result in an ambiguity error, since both functions are equally specific and in the fit of the given input, but because at compile-time we applied the optimization, `f(a: A)` is incorrectly called.
    - To solve this issue, we have to add additional conditions to the optimization. We could, for example, analyze the trait `A` and only apply the optimization if none of `A`'s implementations extend other traits (conservative) or other traits that are also found in the multiple dispatch hierarchy of the multi-function (opportunistic).
