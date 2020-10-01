### TODO

Structs:
- Struct declaration:
  - Implementing any number of traits
    - Compiler ✓
    - Runtime ✓
  - Properties:
    - mutable and immutable ✓
    - delimited by newlines or commas ✓
  - Components
    - Compiler ✓
    - Runtime:
      - Instantiation ✓
      - Dynamic Retrieval ✓
- Construction:
  - call syntax ✓
  - map syntax
    - shorthand (omitting name)
- Ownership ✓

Traits:
- Trait declaration:
  - Inheritance:
    - From traits ✓
    - From component types ✓
    - Inheritance is implicitly passed down the hierarchy ✓
    - Runtime:
      - Subtyping rules ✓
- Ownership ✓

Entities:
- Component Constraints:
  - Immutability ✓
  - Component Type Restrictions ✓
- Ownership:
  - Declaration (structs and entities) ✓
  - Passed down via inheritance, explicitly (re-)declared ✓
  - Access to the owner's type through component types ✓
  - Runtime:
    - Test ownership during struct instantiation. ✓

Cleanup:
- Replace Lists with Vectors. ✓
- Replace assertions with proper CompilationExceptions. Report the position with a CompilationException, if possible.

Performance:
- Provide a sane immutable list implementation.
- Provide a sane immutable map implementation.
- Intern component types.
- Intern struct types and check performance with monster.lore.
- Turn declared type subtyping into a simple HashSet lookup so that we don't need to branch up the supertype tree to decide whether one declared type is the subtype of another. This would be possible by giving each type an exhaustive (transitive) list of supertypes. Downsides might become apparent especially once we introduce dynamic specialization. 

NEXT:
- Finish transpilation and verification for the current MVL constructs.
- THINK: We need to make a fundamental decision: Are list and map types decided at compile-time or at run-time? So, for example, if we have a list of type `[Animal]` but it only has elements of type `Cat`, is the actual type of the list `[Animal]` or `[Cat]`? It seems like it should be the former, but the latter would be more useful, wouldn't it?  
- Then handle further parametric types stuff.

Parametric types:
- Ensure that return types are correctly handled.
- Implement type variables for the remaining phases:
    - Verification: Ensure that type variables are correctly handled during type inference and function verification in general.
    - Transpilation: Ensure that type variables are correctly handled.
- Investigate how type variables affect abstract functions and the ards function.
  - Also mirror this to the wiki.
- Investigate how type variables affect LUB types.
  - Also mirror this to the wiki.
- Define a standard library for lists in some standard Lore file and implement them with dynamic function calls. (We have to use parametric types for this.)
- Add all the concepts to the spec.
  - Make sure to mention that type variable declarations in a function can use preceding type variables. (e.g. where A, B <: A but not where B <: A, A)

Cleanup:
- Mirror the Assignability -> Fits and other changes in the spec wiki.
- Clean up all the commented mess in files such as parametric.lore and TypeVariableAllocation. It'd be best to either cut some of the content or move it to the spec, and only leave the most important comments in the actual code.
    - Affected files: TypeVariableAllocation, Assignability, parametric.lore
- Move more of the spec to the wiki.

Testing:
- Figure out which portions of the compiler and runtime to unit test.
  - We should ideally invest in a system that can test the parts that are replicated in both the compiler and the runtime with the same values. This system should read type relationships from text files and then execute tests. This is crucial because as we discover type system bugs, we should add test cases that cover those bugs. 
- Functional testing: Create a system which automatically runs Lore programs with given arguments and expected outputs. We should amass a large library of Lore programs that together can almost guarantee that the compiler is doing a good job. As we discover bugs, it would be most cost-effective to add additional programs that verify that the compiler or runtime bug has been fixed. 
- Ultimately, we will have a two-layer testing approach:
    1. Unit tests for the most critical components of the runtime and the compiler, especially the type system.
    2. Functional tests for complete Lore programs that test the compiler as a whole, the runtime as a whole, and Pyramid as a whole.
- We probably should also remove the parser tests once we have automated Lore program testing. It neatly covers aspects of the parser. We might design some Lore test programs based on the current parser tests, to catch syntactical strangeness.
- Possibly unit tests for Pyramid?

Next up:
- Tighten and refactor the code. Clear some TODOs. Don't just transpile but also work on the unfinished verification phase.
- Rethink the syntax to be quieter. Noiseless. Less braces and parentheses. Achieve maximum simplicity and clarity.
- Come up with a module system. Implement it.

Parametric Types (Round 2):
- Add type parameters to classes.
- Rudimentary support for right-hand-side sum and intersection types in type allocations?
