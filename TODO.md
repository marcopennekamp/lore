## TODO

#### Features

- Implement symbol types and values:
  - Specification: ~~Symbol types~~, ~~symbol literals~~.
  - Compiler: ~~Symbol types~~, ~~symbol type functions~~, ~~subtyping~~, ~~parser~~, ~~transformer~~, ~~transpilation~~, ~~equality~~, ~~interning~~.
  - Runtime: ~~Symbol types~~, ~~symbol type functions~~, ~~subtyping~~, ~~type equality~~, ~~symbol values~~.
- Implement global constants. Mutable values might follow later, but we absolutely need constants so that certain objects aren't constantly reallocated.
- Implement an append operation for maps. In general, we will need to apply the same run-time typing considerations to maps.
- Implement a module system.
- Implement ranges (see `for comprehensions` in the specification).
- Rethink properties: I don't like how shape properties are orthogonal to multi-functions right now. To use a shape, one is forced to ultimately give a property to an implementing struct. It would be much superior if properties could be declared "virtually", allowing traits to implement properties via some sort of function (perhaps even with dispatch on the accessed type). This feature should also simultaneously solve the question of "virtual/computed properties" posed in the geometry.lore example.
  - This would effectively mean that property types are always changeable and would either bar these kinds of properties to be open or would mean that we'd have to (a) rebuild the type each time the struct is used in dispatch or (b) disable the dispatch cache for multi-functions with shape types. Disallowing "virtual" properties to be open seems like an acceptable compromise, as the other options are far too detrimental on performance.
- A rudimentary form of tree shaking to avoid transpiling functions that aren't used by any other function. This unfortunately requires specifying an entry point. Maybe we could perform tree shaking if such entry points are specified at all.
- Equality and ordering need some love:
  - Well-defined equality and order semantics for all kinds of types.
  - Default equality and order for structs, shapes, tuples, lists, maps, and so on.
    - Shape equality either has to be defined within the compiler or we need a mechanism for iterating over shape properties (basically reflection).
    - What about struct/shape equality? When should a struct be equal to a shape?
    - Both structs and shapes should have no default ordering.

##### Syntax

- Add the `%` to shape types to mirror the literal syntax `%{ ... }`. Consider using square brackets instead of curly braces.
- Turn the map type syntax into `#[A -> B]` to mirror the literal syntax.
- Allow question marks in identifiers. I like how Clojure approaches booleans and this would fit nicely into Lore, I hope. Example: `isSuccessful` would become `successful?`.
- Allow trailing commas.
- We should find a symmetric syntax for map types.
- Allow kebab case? This would be possible if we restrict subtraction to require proper spaces. The downside is that this is potentially very confusing.
  - `hello-world` is an identifier "hello-world".
  - `#syntax-error` is a symbol "syntax-error".
  - `syntax - error` is an arithmetic operation.
  - `syntax -error` is the same arithmetic operation.
  - `syntax- error` are two identifiers "syntax-" and "error".
  - `syntax + -error` is an arithmetic operation, with `error` being negated.

##### Type System

- Support intersection and sum types in TypeVariableAllocation.
- We could theoretically introduce a limited form of ambiguity analysis at compile-time: For each function `f(a: A, b: B, ...)`, get a list of possible subtypes (mostly trait subtypes) and simulate dispatch with these types. If any of the inputs result in an ambiguity, raise at least a warning.    
- Merge Real and Int into a Number type (named Number or something similar). There is no advantage in keeping these two apart when the underlying runtime system has only one numeric type. The subtyping relationship `Int <: Real` is awkward as well.

##### CLI

- *Currently no TODOs.*

##### Error Reporting

- Add positions to CompilationExceptions.
- Add names to errors (similar to Typescript) so that programmers can quickly google/search for Lore errors.
- Transformation phase: If the expression of a variable declaration is incorrect, the variable won't be registered and there will be follow-up errors that may be confusing for a user. There is already code to handle a similar case if the type required of the expression is false. However, the `visitUnary` of the visitor isn't even called when the subtree expression produces compilation errors, so we will have to introduce some other mechanism to the visitor.
- Warn the user if the result type of an if-else expression is Any. This usually suggests an error on the side of the user.

##### Correctness

- Ensure that loops with a Unit expression body cannot be used as an expression, as Unit loops are optimized by the transpiler.
- During loop transpilation, ignore the resulting list if it isn't used at all. This will require allowing expression visitors to query some state from the parent and is possibly complex to implement.


#### Testing

- We probably should remove the parser tests once we have automated Lore program testing. It neatly covers aspects of the parser. We might design some Lore test programs based on the current parser tests, to catch syntactical strangeness. (The biggest argument in favor of throwing out parser testing is that it slows down prototyping syntax heavily. A test that checks some Lore program's output is much easier to write than a unit test relying on the compiler's internal parsing representation.)
- Figure out which portions of the compiler and runtime to unit test.
- We should ideally invest in a system that can test the parts that are replicated in both the compiler and the runtime with the same values. This system should read type relationships from text files and then execute tests. This is crucial because as we discover type system bugs, we should add test cases that cover those bugs. 
  - Idea: The system can be implemented on the compiler side. It would have two parts: (1) immediately executing the typing tests with the compiler subtyping, equality, and fit functions. (2) Compiling the typing tests to Javascript and using the runtime subtyping, equality, and fit functions. This would allow us to reuse the existing type parser even for the runtime tests and also allow us to parse the custom test format using fastparse. 
- Ultimately, we will have a two-layer testing approach:
    1. Unit tests for the most critical components of the runtime and the compiler, especially the type system. Possibly unit tests that test both the compiler and the runtime with the same inputs.
    2. Functional tests for complete Lore programs that test the compiler as a whole, the runtime as a whole, and Pyramid as a whole.
- In the long run, we should build a simple testing framework written in Lore and use it to unit-test Pyramid.
- Add multiple tests that verify that we correctly handle the negative side of totality constraint verification / abstract functions. This is especially important so that changes to the totality constraint checking algorithm don't accidentally lead to illegal programs not being detected anymore. Use the Scala testing environment for this, because the functional tests are not well suited to testing negative compilation outcomes.

##### Benchmarks

- We should leverage the test suite to also run benchmarks to be able to record performance changes when we optimize the compiler. "Real" programs like `dispatch/hello-name.lore` and `combat` would be especially suitable to benchmarking, but probably also artificial cases such as `dispatch/intersection.lore`.


#### Specification

- Add global constants to the specification.
- Clear TODOs in documents: expressions, minimum-viable-language, multi-functions, types.
- Possibly throw away the technical/compiler document, as it is probably massively outdated. Maybe write a shorter summary of the compiler architecture.
- Decide what will happen with the technical/multi-functions document.
- Finish writing the technical/runtime-types document.


#### Code Quality

##### Architecture

- Can we split the type inference phase from the transformation phase?
- Clean up ExpressionTransformationVisitor by moving more functionality to helper objects like ExpressionTransformationHelper.
  - Reconsider some names, as ExpressionTransformation and StatementTransformation aren't similar in functionality even though their names suggest so.
- Move errors to a more central location. It should be easy to see which errors a given phase can produce. This may also allow us to merge some errors.

##### Terminology

- "mutable" should actually be "writeable", since immutability implies that the whole data structure within a property or variable is unchangeable, while we are actually just gating the top-level write access to the property or variable. This is a subtle difference, but it should be considered. Perhaps we can later introduce a "deep" kind of immutability which doesn't just make a property readonly, but actually applies to the whole data structure. As for the keyword, we can just say: `let var x = 5`

##### Clean-Up

- Remove all examples from `/examples/` or turn them into test cases.
- Clean all TODOs within the source code or add them to this TODO list.
- We should reconsider whether positions should be implicit parameters. It's probably better to explicitly state which functions should receive which positions, so that there can be no ambiguity. Implicits are also hard to reason about when we get multiple implicits in nested scopes.


#### Performance

- Compile-time: We can easily implement the following optimization: If the function to be called is a leaf in the hierarchy, i.e. it isn't specialized further, we can call that function directly, because no other functions exist that could specialize the one function contained in the fit. This of course requires whole-program compilation, which is our current approach.
  - Problem: Let's say we have a concrete function `f(a: A)` with `A` being a trait. We also have a trait `Y` and a concrete function `f(y: Y)`. We have a struct `A1 extends A, Y`. The optimization above leads us to call function `f(a: A)` directly at compile-time, given a value of type `A`. However, at run-time, this value is actually an `A1`. Calling `f` directly with it should result in an ambiguity error, since both functions are equally specific and in the fit of the given input, but because at compile-time we applied the optimization, `f(a: A)` is incorrectly called.
    - To solve this issue, we have to add additional conditions to the optimization. We could, for example, analyze the trait `A` and only apply the optimization if none of `A`'s implementations extend other traits (conservative) or other traits that are also found in the multiple dispatch hierarchy of the multi-function (opportunistic). Such an optimization seems too complicated for the MVL, though, especially considering that the language might still change quite a bit.
- Provide a sane immutable list implementation.
- Provide a sane immutable map implementation.
- Runtime: Intern struct types and check performance with monster.lore.
- Runtime: Turn declared type subtyping into a simple HashSet lookup so that we don't need to branch up the supertype tree to decide whether one declared type is the subtype of another. This would be possible by giving each type an exhaustive (transitive) list of supertypes. Downsides might become apparent especially once we introduce dynamic specialization.
  - This is probably not an optimization we want to implement as long as the language is still immature.


#### Editor Support

- Implement a Language Server:
  - https://microsoft.github.io/language-server-protocol/
  - https://code.visualstudio.com/api/language-extensions/overview
  - There are plugins adding LSP support for IntelliJ, so creating a language server seems like the future-facing choice. See: https://github.com/gtache/intellij-lsp.
