## TODO

#### Features

- Implement the new syntax.
  - ~~Add the `%` to shape types to mirror the literal syntax `%{ ... }`.~~
  - ~~Turn the map type syntax into `#[A -> B]`.~~
  - ~~Change keywords: `function` --> `func`, `action` --> `act`.~~
  - ~~Add alternative struct syntax `struct Circle(x: Real, y: Real, radius: Real) extends Shape` that doesn't use a block.~~
  - ~~Allow omitting parameter names in function declarations if the parameter isn't used in a function's body.~~
  - ~~Change block syntax from curly braces to `do...end`.~~
    - ~~Allow omitting parentheses in all control structures.~~
    - ~~Change `if` syntax to `if cond then A else B`. `then` may be omitted if A is an implicit block.~~
    - ~~Change `for` syntax to `for extractors yield E`. `yield` may be omitted if A is an implicit block.~~
    - ~~Change `while` syntax accordingly.~~
    - ~~Start treating newlines in specific contexts (such as after `if condition\n` or `struct X extends A\n`) as `do` portions of a `do...end` block.~~
  - ~~Apply snake_case across the board for variable and function names.~~
  - ~~Allow question marks in identifiers: `empty?`.~~
  - ~~Add `@where` annotation~~.
  - ~~Add `domain` blocks.~~
  - ~~Add pipe operator.~~
  - ~~Add `cond` operator.~~
  - Trailing commas.
  - Implement `object` struct-like singletons.
    - This would allow us to write `object None extends Option[Nothing]` instead of `struct None() extends Option[Nothing]` AND it would remove the need to constantly instantiate Nones: `None` instead of `None()`.
    - Alternative keywords: `instance`/`inst`, `singleton`/`sing`.
  - Clear all `TODO (new syntax)` entries.
- Implement a vector backend for lists.
- Fix map types and values:
  - Add clear covariance/contravariance type semantics.
  - Make maps immutable and support this in the runtime.
  - Implement a clear appends operation for maps and make them generally usable.
- Add immutable (hash) sets with a syntax `#[A]`.
- Implement global constants.
- Implement a module system.
- Implement pattern matching.
- Add further syntactic sugar:
  - Implicit underscore sections (e.g. `map(things, _.name)`).
  - Trailing lambdas.
- Implement some form of ranges for index iteration using `for`.
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

- Down the line, it would be great to have something like:
  ```
  if cond
    tles1
  else
    tles2
  end
  ```
  For now, we require an `end` before the `else` so that an ambiguity like this cannot happen:
  ```
  if cond
    let x = if hello then world
  else
    y
  end
  ```
  Is the `else y` part of the outer or the inner `if`? This is essentially the dangling else problem turned up to 11. Remember that the parser just sees `if abc\n let x = if hello then world\n else\n y\n end`. One solution would be to require a then-style `if` to have its `else` on the same line, but this disallows styles such as this:
  ```
  if cond then hey
  else 
    foo
    bar
  end
  ```
  So we'd need indentation-based blocks to really disambiguate here.

##### Type System

- These concerns apply to Lore as well: https://viralinstruction.com/posts/badjulia/#the_type_system_works_poorly
  - "In Rust, the problem is not even recognizable: Any type you write can freely derive traits."
    I already saw this problem with built-in collection types, such as lists. Say some library has a trait X, but we want lists to implement this trait. That's currently not possible, but quite easy in languages with protocols. The same happens when we have a trait X in library A and a struct Y in library B. It's currently not possible to have Y implement X. It would suffice to specify this at <b>compile time</b> and this is actually quite possible. It's not much different from extending a multi-function defined in library A. We just have to give the option, add runtime support, and make it even harder to implement incremental compilation down the line.
  - A simple syntax would be `impl Trait for Type`. This would add `Trait` as a direct supertype of `Type`. Any abstract multi-functions would have to be implemented accordingly.
  - How we can accomplish this in the runtime for built-in types such as lists and maps is another question. We can, of course, keep a global map of the supertypes of lists, maps, tuples, etc., but this might heavily affect performance.
- The implementation of the Least Upper Bound does some things that intersection/sum type construction should do, too, especially when it comes to combining type arguments. We should investigate whether we should rather implement the LUB in terms of a sum type join (i.e. the reverse to the current situation), with the usual case just being to build a sum type, and joins are applied if necessary/useful.
- Rethink whether multiple type variable mentions at run-time must really be equal, or whether we could go with a softer approach based on finding a "consensus" between occurrences using subtyping. The definition of `fold` for lists is seriously complex. This aspect of the runtime system is not friendly to the daily life of a programmer.
- Add the `expectedType` checking for functions to the inference algorithm. There is no need to check separately, and it would allow the inference algorithm to infer certain types from a function's output type.
- Intersection type construction: Tuple types can be combined: `(A, B) & (C, D) = (A & C, B & D)`. In general, we can normalize covariant and contravariant types: https://dotty.epfl.ch/docs/reference/new-types/intersection-types-spec.html.
  - The `Type.tupled` in the LUB case for functions is a workaround for the lack of tuple combining when constructing intersection types.
- Merge Real and Int into a Number type (named Number or something similar). There is no advantage in keeping these two apart when the underlying runtime system has only one numeric type. The subtyping relationship `Int <: Real` is awkward as well.
- Turn map keys and values into covariant/contravariant type variables if possible.
- Support intersection and sum types in TypeVariableAllocation.
- We could theoretically introduce a limited form of ambiguity analysis at compile-time: For each function `f(a: A, b: B, ...)`, get a list of possible subtypes (mostly trait subtypes) and simulate dispatch with these types. If any of the inputs result in an ambiguity, raise at least a warning.
- Inference:
  - Introduce rewrite rules which come into effect before cycle resolution. One such rewrite rule would be to rewrite judgments such as `(a, b) :<: (c, d)` to `a :<: c` and `b :<: d`. Perhaps this will give the algorithm additional options to continue the inference rather than having to fall back to cycle resolution.
  - `CallTransformation.valueCall` should produce a Fits judgment in one direction (if the right-hand type is not fully instantiated) and a single-direction Subtypes judgment exclusively in the other. The same goes for arguments in MultiFunctionHints.

##### CLI

- *Currently no TODOs.*

##### Error Reporting

- Add positions to CompilationExceptions.
- Add names to errors (similar to Typescript) so that programmers can quickly google/search for Lore errors.
- Transformation phase: If the expression of a variable declaration is incorrect, the variable won't be registered and there will be follow-up errors that may be confusing for a user. There is already code to handle a similar case if the type required of the expression is false. However, the `visitUnary` of the visitor isn't even called when the subtree expression produces compilation errors, so we will have to introduce some other mechanism to the visitor.
- Warn the user if the result type of an if-else expression is Any. This usually suggests an error on the side of the user.

##### Correctness

- We might want to "degrade" the abstractness and totality constraints to warnings. The type system is so complex that the compiler cannot understand all occurrences of abstract types. Sometimes types are not really abstract, but practically/pragmatically abstract through convention. (Such as `Metal[Price]` in `schemas/goods.lore`.) In these cases, we shouldn't force the programmer to define a function that they want to keep abstract. This is rather in the spirit of dynamic languages, but would enrich Lore, I think.
- Ensure that loops with a Unit expression body cannot be used as an expression, as Unit loops are optimized by the transpiler.
- During loop transpilation, ignore the resulting list if it isn't used at all. This will require allowing expression visitors to query some state from the parent and is possibly complex to implement.

##### Transpilation

- MultiFunctionValue: Pull the function value that is created from the multi-function value into the global scope as a constant for the given function, so that it doesn't have to be recreated every time the function is called. (Unless run-time type variable substitutions are necessary.) Alternatively, cache this globally, similar to how we could cache monomorphic types globally.
- FixedFunctionValue: We could transpile one globally accessible function value representation for each function definition so that we don't have to recreate it every time. Alternatively, we could also cache this globally, sort of a "fixed function cache".
- Transpiling single functions (see MultiFunctionTranspiler): The point of transpiling single functions is to bypass multiple dispatch entirely. This is currently possible for a single monomorphic function, but there are ways to treat polymorphic functions in this way too. We just have to assure that a call that is valid at compile-time doesn't become invalid at run-time. This means that none of the variables may have a lower bound, as that might exclude a subtype at run-time only. In addition, type variables may not occur twice or more times in the input type so that missing type equality cannot rule out the validity of the call. Polymorphic functions may still require assigning an argument type to a type variable, but this could then be done ad-hoc in the generated single function. 
- Type aliases could be transpiled such that they can be referred to by name at run time. Currently, type aliases are "thrown away" during scope type resolution. Their type definition gets transpiled as-is. There is a chance to save on type allocations at run time by transpiling type aliases to constants. However, this isn't as simple as it may seem.
  - One idea for transpiling type aliases would be to keep a Type -> Alias map in the registry, which would list for every type with an alias the proper alias. This idea does not stand up to scrutiny. We could for example define two type aliases `A = { name: String }` and `B = { name: String }`. If we come upon a type `{ name: String }`, which may or may not be the result of evaluating an alias, we cannot decide whether to refer to the run-time alias type `A`, `B`, or neither in case no alias type was mentioned in the first place. Hence, we cannot just throw away the information that an evaluated type comes from an alias.
  - Another idea is adding an `alias` property to every type. If that type is actually the definition of an alias, the outer-most type gets its alias property set. For example, if we define an alias `A = { name: String }`, the outer-most shape type gets `A` as its alias property. There are a few gotchas with this approach:
    - Some types actually just refer to some global type instance at compile time. For example, both `String` and say a struct type `Struct` always refer to their unique respective type instance at compile time. So declaring `A = String` should NOT give `String` the alias property `A`. Only constructed types can get the alias property. (Incidentally, this is not so bad, as all types that cannot receive an alias property at compile time are unique variables at run time. The transpiled alias would not be beneficial there.)
    - We will have to make sure that such a property doesn't affect compile-time type equality.
    - In addition, sometimes types may be normalized, such as combined shapes in intersection types, or flattened sums. Such new types shouldn't ever refer to the alias.
  - Yet another approach would be to have the compiler generate run-time constants for types in general, no matter whether they are defined in an alias or not. If the type `#red | #black | #white` occurs multiple times, it would be beneficial to have some constant available at run time that can be referenced, instead of recreating the sum type every time it's referenced. This is trivially possible with monomorphic types and sometimes with polymorphic types, depending on their deduced compile-time value. We already do this for the parameter types in multiple dispatch.


#### Specification

- Add global constants to the specification.
- Clear TODOs in documents: expressions, minimum-viable-language, multi-functions, types.
- Decide what will happen with the technical/multi-functions document.
- Finish writing the technical/runtime-types document. Also move it to the specification folder directly. scopes.md sets a precedent for supplemental documents in the same folder.


#### Testing

- ConstructionSpec: Test SumType.construct.
- Write additional tests in TypeEncoderSpec: sum types, intersection types, tuple types, function types, shape types, symbol types, named types, complex/nested type combinations containing named types. 
- Figure out which portions of the compiler and runtime to unit test.
- We should ideally invest in a system that can test the parts that are replicated in both the compiler and the runtime with the same values. This system should read type relationships from text files and then execute tests. This is crucial because as we discover type system bugs, we should add test cases that cover those bugs. 
  - Idea: The system can be implemented on the compiler side. It would have two parts: (1) immediately executing the typing tests with the compiler subtyping, equality, and fit functions. (2) Compiling the typing tests to Javascript and using the runtime subtyping, equality, and fit functions. This would allow us to reuse the existing type parser even for the runtime tests and also allow us to parse the custom test format using fastparse. 
- Ultimately, we will have a two-layer testing approach:
    1. Unit tests for the most critical components of the runtime and the compiler, especially the type system. Possibly unit tests that test both the compiler and the runtime with the same inputs.
    2. Functional tests for complete Lore programs that test the compiler as a whole, the runtime as a whole, and Pyramid as a whole.
- In the long run, we should build a simple testing framework written in Lore and use it to unit-test Pyramid.
- Add multiple tests that verify that we correctly handle the negative side of totality constraint verification / abstract functions. This is especially important so that changes to the totality constraint checking algorithm don't accidentally lead to illegal programs not being detected anymore. Use the Scala testing environment for this, because the functional tests are not well suited to testing negative compilation outcomes.


#### Code Quality

##### Architecture

- `DeclarationResolver.introspectionTypeDeclarations`: We should refrain from keeping Pyramid optional and just add the Type trait to the core definitions. Then the compiler can just discover the trait and generate the correct API call.
- Rewrite TypeVariableAllocation (compiler) with immutability.
- Can we split the type inference phase from the transformation phase?
- Clean up ExpressionTransformationVisitor by moving more functionality to helper objects like ExpressionTransformationHelper.
  - Reconsider some names, as ExpressionTransformation and StatementTransformation aren't similar in functionality even though their names suggest so.
- Move errors to a more central location. It should be easy to see which errors a given phase can produce. This may also allow us to merge some errors.
- Intern declared types as weak references so that they can be reclaimed if no values use the types.

##### Terminology

- Rename Node to SyntaxNode. DeclNode --> DeclarationSyntaxNode, TypeExprNode --> TypeSyntaxNode, etc.

##### Clean-Up

- Clean most TODOs within the source code or add them to this TODO list.


#### Performance

- Compile-time: We should move type instantiations contained in function bodies into the widest scope possible. Static types should be moved to the global scope and reused. Types that rely on `lore_type_assignments` (such as the list result type in `concat$wcQBQcHEAUI`) should be instantiated once at the beginning of the function.
- Compile-time: There might come a time when the **totality constraint** cannot be fully verified for a particularly complex multi-function. For example, when we have to cycle through an exponentially growing number of concrete subtype combinations, the compiler might choke, making the program virtually uncompilable. A solution to this would be degrading the totality constraint to a warning-producing constraint or alternatively to still produce errors, but without guarantee that all needed functions are implemented at run-time. We could then explore subtype combinations that take too long to check with random samples.
- Compile-time: We can easily implement the following optimization: If the function to be called is a leaf in the hierarchy, i.e. it isn't specialized further, we can call that function directly, because no other functions exist that could specialize the one function contained in the fit. This of course requires whole-program compilation, which is our current approach.
  - Problem: Let's say we have a concrete function `f(a: A)` with `A` being a trait. We also have a trait `Y` and a concrete function `f(y: Y)`. We have a struct `A1 extends A, Y`. The optimization above leads us to call function `f(a: A)` directly at compile-time, given a value of type `A`. However, at run-time, this value is actually an `A1`. Calling `f` directly with it should result in an ambiguity error, since both functions are equally specific and in the fit of the given input, but because at compile-time we applied the optimization, `f(a: A)` is incorrectly called.
    - To solve this issue, we have to add additional conditions to the optimization. We could, for example, analyze the trait `A` and only apply the optimization if none of `A`'s implementations extend other traits (conservative) or other traits that are also found in the multiple dispatch hierarchy of the multi-function (opportunistic). Such an optimization seems too complicated for the MVL, though, especially considering that the language might still change quite a bit.
- Provide a sane immutable list implementation.
- Provide a sane immutable map implementation.
- Runtime: Intern struct types and check performance with monster.lore.
- Runtime: Turn declared type subtyping into a simple HashSet lookup so that we don't need to branch up the supertype tree to decide whether one declared type is the subtype of another. This would be possible by giving each type an exhaustive (transitive) list of supertypes. Downsides might become apparent especially once we introduce dynamic specialization.
  - This is probably not an optimization we want to implement as long as the language is still immature.

##### Benchmarks

- We should leverage the test suite to also run benchmarks to be able to record performance changes when we optimize the compiler. "Real" programs like `dispatch/hello-name.lore` and `combat` would be especially suitable to benchmarking, but probably also artificial cases such as `dispatch/intersection.lore`.


#### Editor Support

##### Language Server

- Implement the following features:
  - Find usages (LSP: references).
  - Rename symbol (LSP: rename, prepareRename).
  - Highlight bindings and types at cursor position (document-wide highlighting of that exact entity).
  - Show type of expression (or at least of a variable).
- Go to definition:
  - The current implementation is very naive, as it can only list global definitions and disregards scopes and shadowing entirely. This is fine for now, but should be improved at some point.
  - Support "go to definition" for members. Members require full knowledge of the instance's type to find all declarations, so we will have to use the compilation result from the registry. This will likely tie into a "usages" extension to the global index.
  - Low priority: For fixed functions, the feature just lists all functions of the multi-function. Obviously we can do better here and only show the functions that are in the min set of the fixed function dispatch by taking type information into account.
- Semantic tokens:
  - Low priority: Try to implement incremental parsing so that we don't have to parse the document fully every time a character changes. This could be accomplished by keeping all nodes for open documents in memory and locally updating nodes with every change.
- Low priority: We should process text document changes with the FragmentChangeHandler.
  - I have already built the fragment change handler, but in hindsight it is not needed for now. The language server rebuilds the index every time a file gets saved, which is frequent enough.
  - Using the FragmentChangeHandler might become more attractive once we need to track local scopes for "find usages" and "rename symbol".

##### Visual Studio Code

- Implement syntax highlighting on the client side.

##### IntelliJ

- Add IntelliJ support via a plugin library like this: https://github.com/ballerina-platform/lsp4intellij.
