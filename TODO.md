### TODO

Parametric types:
- Clean up all the commented mess in files such as parametric.lore and TypeVariableAllocation. It'd be best to either cut some of the content or move it to the spec, and only leave the most important comments in the actual code.
    - Affected files: TypeVariableAllocation, Assignability, parametric.lore
- Investigate how type variables affect abstract functions and the ards function.
- Investigate how type variables affect LUB types.
- Ensure that return types are correctly handled.
- Implement type variables for the remaining phases:
    - Verification: Ensure that type variables are correctly handled during type inference and function verification in general.
    - Transpilation: Ensure that type variables are correctly handled.
- Define a standard library for lists in some standard Lore file and implement them with dynamic function calls. (We have to use parametric types for this.)
- Add type parameters to classes.
- Parse and resolve class type parameters.
- Rudimentary support for sum and intersection types in subtyping for type parameters?
- Add all the concepts to the spec.
    - Make sure to mention that type variable declarations in a function can use preceding type variables. (e.g. where A, B <: A but not where B <: A, A)

Backlog:
- Tighten and refactor the code. Clear some TODOs. Don't just transpile but also work on the unfinished verification phase.
- Finish transpilation and verification for the current MVL constructs.
- Rethink the syntax to be quieter. Noiseless. Less braces and parentheses. Achieve maximum simplicity and clarity.
- Come up with a module system. Implement it.
