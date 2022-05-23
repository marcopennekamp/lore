## TODO

#### Features

- Implement maps: Map values, append for maps.
- Use `lore.core.to_string` when the VM's result is printed in `run_and_print`. This requires the VM to know about `lore.core.to_string` as a core function and, by logical extension, should also include the comparison functions such as `lore.core.equal?`. This would require the respective intrinsics, which currently accept the core functions as lambdas for callbacks, to grab the core functions instead of a lambda.
- Support parallel execution.
  - Clear all `TODO (vm/parallel)` entries.
- Clear all `TODO (vm)` entries.

#### Correctness

- Implement a rudimentary form of instruction validation before execution, such as that no register indices are out of bounds, and possibly simple type checking for constants.
  - Constants accessed via an ID should be checked against the expected variant. For example, if a constant is expected to be an intrinsic, but the actual constant is a value, the VM should quit with an appropriate error (during pre-execution validation) instead of casting the constant to the wrong type and carrying forward the error.

#### Code Quality

- Now that reference comparisons are handled by `===`, we can define `==` as `are_equal` and `<=` as `is_subtype` for Types, and `==` as `are_equal` and `<` as `is_less_than` for TaggedValues.

#### Optimization

- Implement declared type interning:
  - Implement hashing for all types.
    - See the dispatch cache discussion below to reconsider how we can approach hashing.
    - Clear all `TODO (vm/hash)` entries.
  - Cache declared types within a schema.
  - Best intern declared types as weak references so that they can be reclaimed if no values use the types.
  - Clear all `TODO (vm/intern)` entries.
- Implement symbol type/value interning.
  - Symbols can be represented by an integer into a symbol table that is resolved with the universe. We can use one of the unused tag bit patterns to avoid any allocations.
- Consider interning all types. This is a very aggressive strategy, but might actually speed up most common programs, especially those that use a dispatch cache. It would also trivialize subtyping tests for equal types. At the very least, intern constant types.
  - Interning all types will only be useful if multiple dispatch uses a dispatch cache, because the hash code would have to be computed for the cache anyway, so hashing the type for interning is "free". (This is not quite true because dispatch cache hashing could happen lazily, but the point stands anyway.) If the VM does not use a dispatch cache, full type caching would probably not be worth it, since type equality will be checked relatively rarely. The performance benefit to subtyping of equal types will probably not be larger than hashing and interning every single new type. In that case, interning constant types should be the way to go.
  - Of course, regardless of this, declared types should be interned always, as the benefits are too large to ignore. This proposal is talking about function types and tuple types and such, for which the benefit is harder to prove.
- Consider interning constant strings.
- Implement fast persistent lists.
- Implement fast persistent maps.
- Before implementing bytecode JIT compilation, we could try the more specific case of compiling dispatch hierarchies to native dispatch functions, which should vastly improve dispatch performance.
  - To measure dispatch performance properly, we should measure dispatch directly instead of measuring representative programs such as `hello_name.lore`.
- Implement a dispatch cache.
  - Potentially. Jitting dispatch functions might bring better results across the board. My main two worries with dispatch caching are that (1) gauging the cost of plain dispatch vs. a hash table lookup with every function call is quite hard and (2) dispatch caches that never remove "old" entries might grow to very large sizes, without the programmer understanding why the program is eating so much memory. Whereas, caches with expiring entries incur a certain overhead. And additionally, using a dispatch cache in *some* functions incurs a constant run-time overhead *everywhere* because types have to be hashed. Hashing could be carried out lazily, which adds additional overhead, and hashes themselves increase the memory consumption of a type by 8 bytes.
- We can flatten many `ImSeq`s into `UncheckedArray`s or `ImSeqObj`s for types and values (especially tuples, shapes, sum/intersection types, etc.). This will save allocations when creating values, but also make the VM's code a bit more complicated. We'll either have to pass around these embedded ImSeqObj as pointers, or write some glue code here and there.
- Turn declared type subtyping into a simple HashSet lookup so that we don't need to branch up the supertype tree to decide whether one declared type is the subtype of another. This would be possible by giving each type an exhaustive (transitive) list of supertypes. Downsides might become apparent especially once we introduce dynamic specialization.
  - This is probably not an optimization we want to implement as long as the language is still immature.

##### Dispatch Optimization

- We should build a separate dispatch hierarchy for each arity of a multi-function, as arity is trivially mutually exclusive. We could even keep a different dispatch cache per arity. 
- Optimizing single function dispatch: We can rewrite `Dispatch` instructions that have single-function multi-functions as their target to use `Call` instructions. This is easily possible for a monomorphic function, but there are ways to treat polymorphic functions in this way too. We just have to assure that a call that is valid at compile time doesn't become invalid at run time. This means that none of the type parameters may have a lower bound, as that might exclude a subtype at run time. In addition, type parameters may not occur twice in the input type so that missing type equality cannot rule out the validity of the call. Polymorphic functions may still require assigning an argument type to a type variable, but this could then be done ad-hoc in the single function.
- If the function to be called is a leaf in the hierarchy, i.e. it isn't specialized further, we can call that function directly, because no other functions exist that could specialize the one function contained in the fit. This requires poem dispatch instructions to carry compile-time argument type information. 
  - The optimization is quite complex due to the problem mentioned below. However, it's easy to implement for calls where none of the compile-time arguments contain trait or shape types.
  - Problem: Let's say we have a concrete function `f(a: A)` with `A` being a trait. We also have a trait `Y` and a concrete function `f(y: Y)`. We have a struct `A1 extends A, Y`. The optimization above leads us to call function `f(a: A)` directly, given a value of type `A` at compile time. However, at run time, this value is actually an `A1`. Calling `f` directly with it should result in an ambiguity error, since both functions are equally specific and in the fit of the given input, but because at compile-time we applied the optimization, `f(a: A)` is incorrectly called.
    - To solve this issue, we have to add additional conditions to the optimization. We could, for example, analyze the trait `A` and only apply the optimization if none of `A`'s implementations extend other traits (conservative) or other traits that are also found in the multiple dispatch hierarchy of the multi-function (opportunistic).
  - We can generalize this idea to subtrees of the dispatch hierarchy. For example, if we have two root functions `foo(as: [A])` and `foo(as: Option[A])`, we can statically dispatch to the first `foo` if the parameter type is a list, as list and trait types are (currently) mutually exclusive. This would save at least a type check.
- We have to avoid as many subtyping checks as possible. Perhaps the idea of **mutually exclusive types** can be of help here. For example, a 2-tuple and a 3-tuple are always exclusive. There is no way that a value could have both types. The same is true for anonymous functions with different arities. There could be "marker" types which segregate the dispatch search space into small subsets. For example, if we have a function `f(a, b, c)` and `b` and `c` vary wildly, but `a` can be segregated into three different categories, we would *first* check `a`, thereby rule out a huge number of type checks, and then check `b` and `c`.
  - In general, we want to first check the type that rules out the most branches. This can probably be localized to "layers", meaning that we optimize this for each layer of sibling functions with the same specificity. The most crucial is the root layer. However, multi-layer checks may also be necessary to catch most redundant subtyping checks, as local layers might be too conservative and ultimately unnecessary.
  - If we can check a single parameter type first, we can also build a split dispatch cache that works in two or more steps. It would save a lot of memory if for a cached input type `(a, b, c)`, only `a` varies the dispatch result and `b` and `c` don't.
    - A simpler version of this would be to analyze which parameters don't vary the dispatch result AT ALL. This is actually a pretty normal case, especially with basic types. For example, a function `get(a: A, index: Int)` will usually have `Int` as an index type. Similarly, a function like `Stats.base` in the `test/combat` example only dispatches over the second parameter type, as the first parameter type is always `+Stats`.
  - There are probably some restrictions to this when type variables are involved. The great thing about viewing parameters and arguments as tuples is that we can check their fit for each element individually, and out of order. But this only works when these individual types are independent of each other. Type variables complicate this, because they create a "global" interdependence between parameters.
    - That is not to say that we can't check the fit of parameters out of order even with type variables, at least partially. Maybe the magic word here is "rule out". If we have a function whose second parameter is a tuple `(A, Animal)` with `A` being a type variable, we can still rule out this branch if the argument is not a tuple, or if the argument is a tuple whose second element is incompatible with `Animal`.
  - We have to be careful with this, however, as hierarchies aren't always readily apparent. There is no question that a list and a tuple are mutually exclusive, but traits and structs get vastly more complicated with multiple inheritance and, possibly still planned, dynamic specialization.
  - In the spirit of this optimization, we should also add an optimization for how some trait/struct-heavy multi-function hierarchies are handled. There are certainly single-dispatch-like multi-functions which could be compiled down to a lookup table. We just have to identify them.
- The ideal dispatch algorithm would not even delegate to functions like `fits` or `is_subtype` unless absolutely necessary. If we want to check that a given type is a tuple that contains the elements `Animal` and `String`, we don't go `is_subtype(argument_tuple, TupleType(Animal, String))`, we go `argument_tuple.kind == Kind.Tuple and argument_tuple.elements.length == 2 and is_subtype(argument_tuple.elements[0], Animal) and argument_tuple.elements[1] == String`. That's inherently cheaper, because we have to process fewer function calls, fewer match statements, and much more. The motto here is: "Do the same with less!"
  - Of course, this approach requires the VM to generate machine code for the optimized dispatch function at run time, so this isn't quite that easy to implement. Perhaps once we're investing time into JIT compiling the bytecode.
  - We'd combine this with the idea of saving subtyping checks (globally or in each "layer") by identifying the parameter types that even change the dispatch result.
