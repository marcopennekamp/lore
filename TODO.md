## TODO

#### Features

- Finish transformation and transpilation of the current MVL constructs. (WHICH ONES ARE THESE?)

##### Type System

- Introduce a `Unit` type constant, because using `()` is unwieldy.
- Investigate how type variables affect abstract functions and the ARDS function.
- Support intersection and sum types in TypeVariableAllocation.


#### Testing

- Clean up parametric.lore to free up more examples.
- Functional testing: Create a system which automatically runs Lore programs with given arguments and expected outputs. We should amass a large library of Lore programs that together can almost guarantee that the compiler is doing a good job. As we discover bugs, it would be most cost-effective to add additional programs that verify that the compiler or runtime bug has been fixed. 
  - We probably should also remove the parser tests once we have automated Lore program testing. It neatly covers aspects of the parser. We might design some Lore test programs based on the current parser tests, to catch syntactical strangeness. (The biggest argument in favor of throwing out parser testing is that it slows down prototyping syntax.)
- Figure out which portions of the compiler and runtime to unit test.
- We should ideally invest in a system that can test the parts that are replicated in both the compiler and the runtime with the same values. This system should read type relationships from text files and then execute tests. This is crucial because as we discover type system bugs, we should add test cases that cover those bugs. 
- Ultimately, we will have a two-layer testing approach:
    1. Unit tests for the most critical components of the runtime and the compiler, especially the type system. Possibly unit tests that test both the compiler and the runtime with the same inputs.
    2. Functional tests for complete Lore programs that test the compiler as a whole, the runtime as a whole, and Pyramid as a whole.
- In the long run, we should build a simple testing framework for Lore and use it to unit-test Pyramid.


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

- Provide a sane immutable list implementation.
- Provide a sane immutable map implementation.
- Runtime: Intern component types.
- Runtime: Intern struct types and check performance with monster.lore.
- Runtime: Turn declared type subtyping into a simple HashSet lookup so that we don't need to branch up the supertype tree to decide whether one declared type is the subtype of another. This would be possible by giving each type an exhaustive (transitive) list of supertypes. Downsides might become apparent especially once we introduce dynamic specialization. 
