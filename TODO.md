## TODO

#### Features

- Finish transformation and transpilation of the current MVL constructs. (WHICH ONES ARE THESE?)
  - Ranges still need to be supported, as they are already part of the specification.
- Allow trailing commas.

##### Type System

- Introduce a `Unit` type constant, because using `()` is unwieldy.
- Investigate how type variables affect abstract functions and the ARDS function.
- Support intersection and sum types in TypeVariableAllocation.

##### CLI

- Support compiling files using a pattern instead of just plain filenames.
- Support an output file other than `lore-program.js`.


#### Testing

- Clean up parametric.lore to free up more examples.
- We probably should remove the parser tests once we have automated Lore program testing. It neatly covers aspects of the parser. We might design some Lore test programs based on the current parser tests, to catch syntactical strangeness. (The biggest argument in favor of throwing out parser testing is that it slows down prototyping syntax heavily. A test that checks some Lore program's output is much easier to write than a unit test relying on the compiler's internal parsing representation.)
- Figure out which portions of the compiler and runtime to unit test.
- We should ideally invest in a system that can test the parts that are replicated in both the compiler and the runtime with the same values. This system should read type relationships from text files and then execute tests. This is crucial because as we discover type system bugs, we should add test cases that cover those bugs. 
- Ultimately, we will have a two-layer testing approach:
    1. Unit tests for the most critical components of the runtime and the compiler, especially the type system. Possibly unit tests that test both the compiler and the runtime with the same inputs.
    2. Functional tests for complete Lore programs that test the compiler as a whole, the runtime as a whole, and Pyramid as a whole.
- In the long run, we should build a simple testing framework written in Lore and use it to unit-test Pyramid.
- Add multiple tests that verify that we correctly handle the negative side of totality constraint verification / abstract functions. This is especially important so that changes to the totality constraint checking algorithm don't accidentally lead to illegal programs not being detected anymore. Use the Scala testing environment for this, because the functional tests are not well suited to testing negative compilation outcomes.

##### Benchmarks

- We should leverage the test suite to also run benchmarks to be able to record performance changes when we optimize the compiler. "Real" programs like `dispatch/hello-name.lore` would be especially suitable to benchmarking, but probably also artificial cases such as `dispatch/intersection.lore`.


#### Specification

- Clear TODOs in documents: expressions, minimum-viable-language, multi-functions, types.
- Possibly throw away the technical/compiler document, as it is probably massively outdated. Maybe write a shorter summary of the compiler architecture.
- Decide what will happen with the technical/multi-functions document.
- Finish writing the technical/runtime-types document.


#### Clean-Up

- Clean all TODOs within the source code or add them to this TODO list.
- Replace assertions with proper CompilationExceptions. 
- Add positions to CompilationExceptions.


#### Performance

- Compile-time: We can easily implement the following optimization: If the function to be called is a leaf in the hierarchy, i.e. it isn't specialized further, we can call that function directly, because no other functions exist that could specialize the one function contained in the fit. This of course requires whole-program compilation, which is our current approach.
- Provide a sane immutable list implementation.
- Provide a sane immutable map implementation.
- Runtime: Intern component types.
- Runtime: Intern struct types and check performance with monster.lore.
- Runtime: Turn declared type subtyping into a simple HashSet lookup so that we don't need to branch up the supertype tree to decide whether one declared type is the subtype of another. This would be possible by giving each type an exhaustive (transitive) list of supertypes. Downsides might become apparent especially once we introduce dynamic specialization. 
