## TODO

#### Features

##### Minimum Viable Language

- Syntax changes:
  - In the process: Move to Scala 3 and migrate to a different parser library.
  - Move to a full indentation-based syntax. I think this provides the best consistency in the long run. There are already weird syntax rules around if-else (special rule around multi-line/single-line), modules (forced `do`), actions (forced `do`) and functions, cond (`do..end` for bodies with more than one expression), and so on. Significant indentation can fix all of these issues, while keeping the syntax consistent and with less noise overall.
  - Change `!`, `&&` and `||` to `not`, `and`, and `or`. Especially `!` is weird with the ability to put `?` or `!` into a function name: `!equal?(a, b) || !check?!(x)` vs. `not equal?(a, b) or not check?!(x)`.
  - Remove automatic casts between Real and Int and exclusively rely on `to_int` and `to_real` functions. This removes one of Lore's biggest uncertainties for the user and thus hopefully a source of errors. Int literals should also not be usable as Real literals, i.e. `5` should never be typeable as a Real.
    - Also remove automatic promotion of `Int` to `Real` in default comparisons and separate them in the type kind order.
  - Single-line version of loops: `while <condition> do <body>` instead of `yield`. I want to reserve `yield` for later usage.
  - Rename `act` to `proc`. This would be in line with `func`.
    - `act` could also be confused with `actor` (actor models, etc.), leading someone new to the language to think that the function somehow supports or enables concurrency via the actor model.
  - Rename `let mut` to `var`.
  - Rename `let` to `val`? 
    - `private let` for global values reads weirdly in comparison to `private val`. Yet I prefer `let` in function bodies.
  - Replace `extends` with `:` (like in Kotlin) or `<:`. `extends` is too noisy, in hindsight. (And also annoying to parse with preceding and succeeding whitespace.)
    - Also take into accounts cases where `:`/`<:` should be placed on the next line. This could be a problem given significant indentation.
  - Allow `else` as the `true` cond case.
  - Rename `Boolean` to `Bool`. Int is also abbreviated.
  - Consider renaming `<-` in `for` loops to `in`.
  - Remove map construction syntax for structs and marry this with the call syntax. The call syntax should be able to work with default values in structs. Also find a way to create a struct from a shape value.
  - Collection syntax:
    - Maps: `#['Alex' -> 22, 'Mary' -> 17]`
    - Sets: `#['Alex', 'Mary']`
  - Consider simplifying shape syntax to `{ ... }` instead of `%{ ... }`.
  - Provide a means to directly access tuple elements, such as `._1`.
    - Also consider adding default element names again, i.e. `tuple.a` for a tuple `(A, B)` referring to the first element. Might still be a slippery slope (especially for tooling/renaming).
  - Implicit underscore sections (e.g. `map(things, _.name)`) or an equivalent shortcut syntax (such as `it`).
  - Trailing lambdas (`map(things) do thing => thing.name`).
    - Take care that the syntax doesn't require a `do` after `thing =>`, e.g. `map(things) do thing => do ...` would be unfortunate.
  - Introduce a general symbol type that supertypes all symbol types and can be used for functions such as `lore.symbol.name`, other Pyramid and reflection functions, and inside the compiler to replace `Type.isSymbol`.
  - Consider adding indentation-aided parsing at this point, before introducing `case` expressions, as those would majorly benefit from indentation-aided parsing.
  - Allow double quotes `"` for strings alongside single quotes. Double quotes are so natural for strings that we will never be able to use them for any other kind of syntax, without majorly confusing every programmer in existence (myself included).
    - Other uses for single quotes:
      - Identifier characters (like Haskell).
    - I like the aesthetics of single quotes, though...
  - Allow string concatenation with `+`, at least until operator overloading is supported.
  - Support private module members with the `private` keyword.
  - New comment syntax: `--` and `[-- --]`.
    - Possible divider styles:
      ```
      ------------------------------------------------------------------------------------------------------------------
      -- Divider heading                                                                                              --
      ------------------------------------------------------------------------------------------------------------------

      ------------------------------------------------ Divider heading -------------------------------------------------

      -- ---------------------------------------------------------------------------------------------------------------
      -- Divider heading (Haskell style)
      ```
  - Make sure that all keywords are forbidden, including those that were previously deemed "soft" keywords.
  - Direct list imports such as `use foo.[bar, foo, baz]` are currently resolved as `use foo.bar; use foo.foo; use foo.baz`. This is obviously incorrect, because `baz` should refer to `foo.baz` not `foo.foo.baz`. We should either resolve list imports without unfolding their structure, or require the list import to not refer to its head segment in any of the imported bindings.
  - Clear all `TODO (syntax)` entries.
- Disjunction and conjunction operations aren't short-circuiting. This was probably an oversight when we moved from Javascript to the VM. Reimplement this feature.
  - In general, a short-circuiting operator `a || b` can be compiled as `if a then true else b` and `a && b` as `if a then b else false`. A naive implementation of short-circuiting operators would be to simply replace `and` and `or` with their respective `if` expansions.
    - Without much effort, this will lead to inefficient bytecode. For example:
      ```
      if a == b && c == d then 10 else 20
      ```
      This would be naively compiled as:
      ```
      if (if a == b then c == d else false) then 10 else 20
      ```
      This results in bytecode that first calculates the inner expression into a temporary variable, and *then* uses it to jump via the outer `if`, while already using jumps to calculate the and's result. This results in the following bytecode (assuming the variables are globals):
      ```
       0: GlobalGet reg0 <- a
       1: GlobalGet reg1 <- b
       2: IntEq reg0 <- reg0 reg1
       3: JumpIfFalse 8 if !reg0
       4: GlobalGet reg0 <- c
       5: GlobalGet reg1 <- d
       6: IntEq reg0 <- reg0 reg1
       7: Jump 10
       8: BooleanConst reg1 <- false
       9: Assign reg0 <- reg1
      10: JumpIfFalse 13 if !reg0
      11: IntConst reg0 <- 10
      12: Jump 15
      13: IntConst reg1 <- 20
      14: Assign reg0 <- reg1
      15: Return reg0
      ```
      Note how instructions 6 and 8+9 assign either `c == d` or `false` to a temporary boolean register. Instruction 10 consumes this register to implement the jump for the outer `if`, but the jumps could've simply been carried out right away. An optimized version of this would be (fewer instructions and one jump less):
      ```
       0: GlobalGet reg0 <- a
       1: GlobalGet reg1 <- b
       2: IntEq reg0 <- reg0 reg1
       3: JumpIfFalse 10 if !reg0    // Jump directly to the `else` part of the outer `if`.
       4: GlobalGet reg0 <- c
       5: GlobalGet reg1 <- d
       6: IntEq reg0 <- reg0 reg1
       7: JumpIfFalse 10 if !reg0    // Jump directly to the `else` part of the outer `if`.
       8: IntConst reg0 <- 10
       9: Jump 11
      10: IntConst reg0 <- 20
      11: Return reg0
      ```
      I see two approaches here: (1) improve code generation by handling the edge case `if (if ...)` (or the `cond` equivalent) specially or (2) implement bytecode optimizations that discover the redundant temporary boolean register and "merge" the jumps accordingly. My gut tells me that (1) is much easier, but (2) will be more generally applicable. Surely there will be an existing optimization strategy which we can look up that handles exactly these cases.

      One bit of insight: If `3: JumpIfFalse` jumps to 8, `10: JumpIfFalse` ALWAYS jumps to 13. When operating on the control-flow graph of the bytecode, it's probably possible to follow jumps and see if jumping just takes them to another jump that's guaranteed. In this case, the "difficulty" for the optimizer is to notice that `10: JumpIfFalse` will always jump if the block is entered at 8. This is trivial, though, if 8-10 is seen as one block in the control-flow graph, as this block could be simplified to `Jump 13` and then be removed. Similarly, for the `true` case, the optimizer should see that `7: Jump 10` leads to a block with a single instruction `JumpIfFalse 13`, and thus the `Jump 10` can be replaced by the `JumpIfFalse 13`.
    - Expanding to `if` might have a bad interaction with `if` being internally represented as a special case of `cond`. It should work fine, but when implementing this feature, double-check that `cond` exhibits the same short-circuiting behavior.
  - If the right-side expression `b` is without side effects, `a && b` and `a || b` is probably better compiled to `BooleanOr` unless `b` is very complex. The question is how much of this should be optimized by the compiler and how much the VM can do.
- Improve dispatch consistency:
  - Dispatch should *never* invoke a function that would return an incompatible return type. Lower bounds for function type parameters combined with multiple inheritance sadly completely enable this. Consider the example `test/language/dispatch/lower_bound_confusion.lore`. The compiler can easily be tricked into thinking a returned `String` would actually be an `Int`! There are a few ways to approach this:
    - Nuclear option: Remove lower bounds from *(multi-)function* type parameters. Structs and traits could still easily have lower bounds, because struct constructors aren't beholden to issues arising from dispatch. However, this would rule out many legitimate uses of lower bounds.
      - Of course, there is *always* the risk of a lower bound leading to an empty-fit error at run time. The issue is when code *continues* to execute unabated even though it should've errored out. The saving grace for multiple inheritance is currently that almost all conflicts result in run-time ambiguity errors, but lower bounds subvert this "guarantee".
      - The negative impact of removing lower bounds from multi-functions would be lessened by the introduction of "def" functions that are single-function only and would be able to have lower bounds for type parameters. Similar to struct constructors, the unexpected effects due to dispatch don't occur here, as these functions would be called directly.
      - TypeScript doesn't have lower bounds: https://github.com/Microsoft/TypeScript/issues/14520
    - Completeness option: Declaring a function such as `describe(animal: A)` is inherently an *incomplete* function definition, because its domain does not cover ALL values that `A` could take. Perhaps it's possible to have the compiler enforce that a function with a lower bound must additionally be covered by a second function (without a lower bound) that guarantees its completeness for all argument types. In the current example, this would be a function `describe(animal: B)` with `Nothing <: B <: Any`. This second function should be a *superfunction* of the first function, so for `Mammals` and above (an empty set btw) the first function will specialize the second function.
      - I like this one. Hopefully it's possible.
      - How does this interact with type variables?
    - Analysis option: Disallow a constellation like in `lower_bound_confusion.lore` through analysis of type hierarchies, either for whole multi-functions or given each individual call (the latter is probably untenable from an efficiency standpoint).
      - Multi-function analysis: Check all concrete subtypes of `animal: A` and their min set in dispatch. The compiler will find that `Goldfish` actually has a min set of `describe(scaly: Scaly): String` and thus `describe(animal: A)` should have an output type that is compatible with `String`, e.g. `Int | String`. Mark this as an error in `describe(animal: A): Int`.  
      - Call-site analysis: `animal` could feasibly contain a value of type `Goldfish`, which would make `describe(animal)` return a `String` in some cases. Thus, mark the call as an error.
      - Both of these analysis approaches run into the same issues as the totality constraint: we need to check all concrete subtypes, but the set of such types can grow very quickly. The constraints might also be too narrow and disallow various constellations that would usually be allowed.
      - Can we analyse this without going through all concrete subtypes? How does this issue evolve when *multiple* arguments are involved? Does the analysis even terminate in all cases?
    - Philosophical option: For what do we need lower bounds? Go back to the drawing board. Perhaps we only need type variables as lower bounds? Do type variable exhibit the same issues? Do concrete-type lower bounds even make sense as only structs are actually concrete? Perhaps for shapes?
    - Run-time checking option: The core issue is that the function's run-time output is incompatible with its compile-time output type. It should additionally be noted that most of these incorrect uses of lower bounds are pathological, i.e. likely won't occur very often in real-world code that is reasonably designed. If (exhaustive) compile-time analysis is not possible and we still want to offer lower bounds as a feature, it might be necessary to introduce run-time checks for output types. This would only be enabled per call site if a multi-function path provably contains a lower bound, for example.
    - Julia offers lower bounds in multi-functions, but currently has no multiple inheritance. This discussion is insightful, however: https://github.com/JuliaLang/julia/issues/5. The "common descendant" restriction they formulate throughout the issue is an interesting starting point for the analysis approach, maybe.
  - Clear all `TODO (dispatch-consistency)` entries.
- Fix map types and values:
  - Add clear covariance/contravariance type semantics.
  - Make maps immutable and support this in the runtime.
  - Implement a clear appends operation for maps and make them generally usable.
  - Change the map type syntax to `#[K, V]` which is more convenient to type. The literal syntax should keep the `k -> v` syntax because the alternative `k: v` signals to the programmer that `k` is a name, not a value. So if we write `#[foo: bar]`, the programmer might assume that `'foo'` is the key with which the value is accessed, even when in reality `foo` is defined `let foo = 15`.
  - Update `runtime-types.md`.
  - Clear all `TODO (maps)` entries.
- Add `case` expressions and pattern matching in lambda function parameters, variable declarations, and the left-hand side of assignments (e.g. for assigning tuple values to mutable variables).
  - Clear all `TODO (case)` entries.
- Allow calling a function `f` with an argument `v: A | B` if both `v: A` and `v: B` can be successfully dispatched. This essentially removes the need to declare superfunctions such as `hello(String | Int)`, not only bringing a benefit in succinctness, but also making extending existing library functions easier. It also makes working with sum types easier.
- Refactor Pyramid and add more types and functions.
  - What happens if we put `Type` values into `to_string`, `equal?`, and `less_than?`? All of these should work, with equality and order deferring to type equality and subtyping.
  - Clear all `TODO (pyramid)` entries.
- Overhaul shapes/shape types, "virtual" properties, and any related features. I think shapes are still too limited. The crux is this: To match on a shape, a type needs to have the property backed as an actual, physical struct property. Many possibilities will open up if virtual properties can be matched by shapes as well, because then arbitrary types (e.g. traits high up in a hierarchy) can be "extended" to work with a particular shape. Also see the corresponding thoughts "Lore: Project Struct/Trait" in ColorNote. (I know. I'll move this eventually to a proposal.)

##### MVL improvements

- Implement iterators as a first-class feature with the keyword `iter`. Iterators should be able to be passed around as values or to be wrapped in another type (such as a Sequence or Stream type), which would also be able to wrap lists and maps. Iterators should also be able to be inlined, for example when directly used in a `for` loop. We'll have to see how to reconcile the concepts of a `yield` in an iterator and a `yield` in a for/while. Maybe we can combine these concepts somehow.
  - An alternative would be a template/macro system, but this would only cover the inline uses of an iterator. Maybe the inline iterator and an iterator as a value are two different concepts which Lore needs both.
    - `inline` could be an annotation for `def` functions, similar to [https://dotty.epfl.ch/docs/reference/metaprogramming/inline.html](https://dotty.epfl.ch/docs/reference/metaprogramming/inline.html).
  - Also implement some form of ranges for index iteration using `for`.
  - The goal should be to have a sort of "Enum" interface against which general sequence functions can be written.
  - Are transducers applicable here? (I think they're quite hard to put into a language with a static type system, but we should explore this idea more.)
- Implement a new backend for lists.
  - Clear all `TODO (lists)` entries.
- Add immutable (hash) sets with a syntax `#[A]` or `{A}`.
- Provide an easy way to update immutable structs and shapes. For example, see Scala's case class `copy` function.
- Allow inherited shape type properties to reference declared types placed lower in the schema initialization order. The reasoning for this is simple: Struct properties are immune to the initialization order, because they are initialized in a second step. This allows structs to include each other as properties. Inherited shape types essentially specify the properties of a trait, so they should enjoy the same privileges. There is nothing but complexity that keeps us from realizing the resolution of inherited shape types in a second step.
  - The VM already handles this correctly.
- Add "global specialization"/"trait implementation" for tuples, lists, maps, shapes, traits, and structs.
  - This will allow us to type lists, for example, as Enums, and so on.
- Possibly add protocols. (Also see the specification proposal.)
  - This will allow us to add equality, ordering, hashing, and stringification protocols to the core, which makes the operations associated with these protocols more type-safe. For example, right now, it is possible to compare values of any two types, even those that are incomparable (e.g. `function == struct`).
  - Protocols might be mergeable with "global specialization".
- Allow non-equality (`!=`) to be implemented separately, as non-equality can sometimes be proven more quickly than equality.
- Introduce a `Number` type that supertypes both `Int` and `Real` (possibly just `type Number = Int | Real`) and that can be used for arithmetic operations. The exact semantics of such a type have to be figured out, but the easiest would be implicit conversions from `Int | Real` to `Real`, which could be supported by amending the `IntToReal` instruction such that it's idempotent if `Real` values are passed to it.
  - The motivation for a `Number` type would be defining math functions, for example, but this requires specialization instead of implicit conversion. For example, a function `func max[N <: Number](a: N, b: N): N = if a > b then a else b` would need to be specialized for `Int` and `Real`, or alternatively work with an implementation of `>` that delegates to the correct instruction based on `N`. And then there is performance, as type parameters are notoriously slow to handle in Lore's multiple dispatch. So this would require a separate compile-time specialization mechanism, which perhaps isn't worth the effort given that we only have two number types.
- Overhaul multi-reference name resolution and imports:
  - Dividing multi-references into local and global layers seems quite hacky, as it "squashes" the usual scoping and name resolution rules. Perhaps we can look at Kotlin's "tower" name resolution approach (and especially its static overload resolution) for inspiration.
  - Imports only being available at the module level is overly restrictive. Imports should be available in blocks. Again, look at Kotlin's tower strategy for inspiration.

##### Syntax

- Indentation-aided parsing:
  - Significant indentation is not intended to replace `do..end`, but rather aid in parsing ambiguities. In that sense, we are calling this feature "indentation-aided parsing".
    - We should also consider removing `do..end` entirely and moving to significant indentation altogether. This might ultimately make the code much cleaner and clear out any inconsistencies about when to use `do..end` and when to skip it.
  - This would resolve some issues with `do..end` inconsistencies, such as:
    - Block-style objects and modules requiring the `do` keyword due to parsing ambiguities.
    - A then-style `if` requiring the `else` to occur on the same line as the end of the "on true" top-level expression.
    - `cond` and `case` requiring `do..end` for blocks, which adds serious visual noise.
    - Concrete actions require the `do` keyword to disambiguate them from abstract actions. With significant indentation, this `do` requirement would only be needed for empty actions, where indentation-aided parsing cannot rely on an indented expression. On the other hand, the `do..end` syntax is currently consistently applied to block functions and actions. We might want to keep this requirement for stylistic reasons.

##### Traits, Structs, and Shapes

- Rethink properties: I don't like how shape properties are orthogonal to multi-functions right now. To use a shape, one is forced to ultimately give a property to an implementing struct. It would be much superior if properties could be declared "virtually", allowing traits to implement properties via some sort of function (perhaps even with dispatch on the accessed type). This feature should also simultaneously solve the question of "virtual/computed properties" posed in the `geometry.lore` test.
  - This would effectively mean that property types are always changeable and would either bar these kinds of properties to be open or would mean that we'd have to (a) rebuild the type each time the struct is used in dispatch or (b) disable the dispatch cache for multi-functions with shape types. Disallowing "virtual" properties to be open seems like an acceptable compromise, as the other options are far too detrimental on performance.

##### Type System

- These concerns apply to Lore as well: https://viralinstruction.com/posts/badjulia/#the_type_system_works_poorly
  - "In Rust, the problem is not even recognizable: Any type you write can freely derive traits."
    I already saw this problem with built-in collection types, such as lists. Say some library has a trait X, but we want lists to implement this trait. That's currently not possible, but quite easy in languages with protocols. The same happens when we have a trait X in library A and a struct Y in library B. It's currently not possible to have Y implement X. It would suffice to specify this at <b>compile time</b> and this is actually quite possible. It's not much different from extending a multi-function defined in library A. We just have to give the option, add runtime support, and make it even harder to implement incremental compilation down the line.
  - A simple syntax would be `impl Trait for Type`. This would add `Trait` as a direct supertype of `Type`. Any abstract multi-functions would have to be implemented accordingly.
  - How we can accomplish this in the runtime for built-in types such as lists and maps is another question. We can, of course, keep a global map of the supertypes of lists, maps, tuples, etc., but this might heavily affect performance.
- Reintroduce a "least upper bound"/join for complex sum types for select expressions, such as list and map constructions.
  - This is mostly an optimization to avoid complex list value types at run time.
- Support intersection and sum types in subtyping/equality unification.
- We could theoretically introduce a limited form of ambiguity analysis at compile-time: For each function `f(a: A, b: B, ...)`, get a list of possible subtypes (mostly trait subtypes) and simulate dispatch with these types. If any of the inputs result in an ambiguity, raise at least a warning.
- We may need to remove open property types from struct types in run-time type variable assignments. A motivating example would be `lore.list.concat`. We probably don't want the resulting list type to contain any open properties, provided that one of the type arguments contains a struct type with open property types. Reproducing this is probably a bit harder than one might expect, because most list types are set at compile time. But we could for example create a list via a function `func single(element: A): [A] where A = [element]`. This might type the run-time list with the element's actual type, thereby leaking a possible struct's open property types into the list. The solution would be to strip any open properties from struct types inside run-time type variable assignments.
  - We have to think carefully about this, though. There might be instances where we **want** the type argument to have its open property types.
  - I think the VM already does this. Need to verify.

##### CLI

- *Currently no TODOs.*

##### Error Reporting

- Add positions to CompilationExceptions.
- Possibly sort errors by the phase they were raised in. Due to the non-stopping nature of the compiler, some errors only occur because of other errors. For example, a struct alias aliases an object, but is declared as `struct`, and then later a property of the "object" cannot be accessed because the compiler thinks it's a struct, causing confusing error messages.
  - Alternatively, offer a stopping mode (phase-by-phase or even more granularly) for CLI compilation and non-stopping mode for IDE compilation.  
- Add names to errors (similar to Typescript) so that programmers can quickly google/search for Lore errors. We might also add links to a specific error documentation (or wiki) on the web to keep compiler error messages short, but still allow beginners to read an extended version of the error, along with examples (DOs and DONTs). 
- Transformation phase: If the expression of a variable declaration is incorrect, the variable won't be registered and there will be follow-up errors that may be confusing for a user. There is already code to handle a similar case if the type required of the expression is false. However, the `visitUnary` of the visitor isn't even called when the subtree expression produces compilation errors, so we will have to introduce some other mechanism to the visitor.

##### Correctness

- We might want to "degrade" the abstractness and totality constraints to warnings. The type system is so complex that the compiler cannot understand all occurrences of abstract types. Sometimes types are not really abstract, but practically/pragmatically abstract through convention. (Such as `Metal[Price]` in `schemas/goods.lore`.) In these cases, we shouldn't force the programmer to define a function that they want to keep abstract. This is rather in the spirit of dynamic languages, but would enrich Lore, I think.
  - There might also come a time when the **totality constraint** cannot be fully verified for a particularly complex multi-function. For example, when we have to cycle through an exponentially growing number of concrete subtype combinations, the compiler might choke, making the program virtually uncompilable. We might need to implement upper bounds on the number of subtypes tested.


#### Testing

- Test multi-function disambiguation for multi-function calls, multi-function values, and fixed functions. Both the happy path and potential errors should be considered. Attention should also be paid to the interplay between local and global availability.
- Spec execution (VM):
  - Sort specs alphabetically by description to achieve a consistent test execution order.
  - Break words in reported spec descriptions according to the terminal size and indent them after the `okay`/`fail` to improve readability of test/benchmark results.
- When Scala tests are executed, Pyramid is compiled freshly for each `.lore` test source. This scales badly. Maybe we can exclude Pyramid from Scala tests that don't import any of its functionality.
- We should invest in a system that can test type system functions in both the compiler and the VM with the same values.
  - This system could read types from a `.lore` file and then a type relationship specification from a text file. This is crucial because as we discover type system bugs, we should add test cases that cover those bugs to both implementations.
  - Such a system is only advisable once we have the compiler written in Lore. Otherwise we're duplicating work which doesn't directly go towards building the compiler in Lore.


#### Code Quality

##### Architecture

- Move errors to a more central location. It should be easy to see which errors a given phase can produce. This may also allow us to merge some errors.

##### Terminology

- Rename "sum types" to "union types" for better name duality with "intersection types".
- Rename IR trees for more consistent naming:
  - `*Node` to `Ast*`. DeclNode --> AstDeclaration, TypeExprNode --> AstType, etc.
  - `Untyped*` as is? Or abbreviation?
  - `*Expression` to `Typed*`, maybe. `Expression` definitely renamed to `TypedExpression`.

##### Clean-Up

- Clean most TODOs within the source code or add them to this TODO list.


#### Editor Support

##### Language Server

- Fix the language server given the recent changes.
- Implement the following features:
  - Find usages (LSP: references).
  - Rename symbol (LSP: rename, prepareRename).
  - Highlight bindings at cursor position (document-wide highlighting of that exact entity).
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
  - Jetbrains Fleet will support LSP. So maybe wait for that.


#### Example Programs

In general, Lore needs more example programs that cover and use language features in different ways. This helps us discover elements of friction in the language design, find bugs, and provide a larger base of examples to new users.

- Write some Lore programs that solve advent of code puzzles: https://adventofcode.com
- Implement Julia micro-benchmarks: https://github.com/JuliaLang/Microbenchmarks
