## TODO

#### Features

##### Minimum Viable Language

- Implement specs:
  - Compiler: ~~Specs in the Registry~~, ~~parsing~~, ~~resolution~~, ~~constraints~~, ~~transformation~~, ~~assembly~~.
  - VM: ~~Spec definitions~~, ~~poem reading and writing~~, ~~universe resolution~~, ~~`lore.test.assert` intrinsic~~, ~~`test` and `bench` commands~~, ~~module name filtering~~.
  - Pyramid: ~~`lore.test` module with assertion functions~~.
  - Functional tests in `test` should all be compiled into one binary. To that end, each test must be correctly wrapped in its own module.
    - This allows the compiler to run once, removing the need to create a native image from the compiler JAR. The VM would also need to be run only once using the `test` command, achieving further test performance gains.
    - Some tests should be moved into Pyramid itself.
    - Some tests such as `hello_name.lore` and `combat` should be marked as benchmarks. We could also add additional benchmarks.
  - Clear all `TODO (specs)` entries.
- What happens if we put `Type` values into `to_string`, `equal?`, and `less_than?`? All of these should work, with equality and order deferring to type equality and subtyping.
- Fix map types and values:
  - Add clear covariance/contravariance type semantics.
  - Make maps immutable and support this in the runtime.
  - Implement a clear appends operation for maps and make them generally usable.
  - Change the map syntax to avoid clashing with hash sets.
  - Update `runtime-types.md`.
  - Clear all `TODO (maps)` entries.
- Allow multiple multi-function imports with the same name from different modules and implement compile-time disambiguation of such multi-function calls.
  - This would also allow us to introduce a `list.length`-style function call syntax (even with optional parentheses for functions with no additional parameters). It might make type inference harder, though.
  - Refactor Pyramid such that "object-domain" functions are in the same module as their type. For example, a function `get!` for `Option` should be in the same module `lore.option`, so that we have `lore.option.Option` and `lore.option.get!` that can be imported with a single wildcard import.
    - The explanation of companion modules in `modules.md` should mention that companion modules should contain functions for constructing instances of the type, such as `lore.list.List.repeat`, or members that are otherwise "static" to the type, such as various constants.
- Syntax changes:
  - Change `!`, `&&` and `||` to `not`, `and`, and `or`. Especially `!` is weird with the ability to put `?` or `!` into a function name: `!equal?(a, b) || !check?!(x)` vs. `not equal?(a, b) or not check?!(x)`.
  - Possibly allow chaining comparison operators, e.g. `a <= b <= c` parsed as `a <= b && b <= c`.
  - Rename `act` to `proc`? This would be in line with `func`.
  - Rename `Boolean` to `Bool`? Int is also abbreviated.
  - Implement implicit real conversions for integer literals standing in `Real` contexts.
    - A list like `[0, -2, 2.5, 6, 22]` should also be typed as `[Real]`. Even a list `[1, 2, 3]` may be typed as `[Real]` if a `Real` list is expected in context.
  - Provide a means to directly access tuple elements, such as `._1`.
    - Also consider adding default element names again, i.e. `tuple.a` for a tuple `(A, B)` referring to the first element. Might still be a slippery slope.
  - Implicit underscore sections (e.g. `map(things, _.name)`) or an equivalent shortcut syntax.
  - Trailing lambdas.
  - Introduce a general symbol type that supertypes all symbol types and can be used for functions such as `lore.Symbol.name`, other Pyramid and reflection functions, and inside the compiler to replace `Type.isSymbol`.
  - Consider adding indentation-aided parsing at this point, before introducing `case` expressions, as those would majorly benefit from indentation-aided parsing.
  - Consider adding `func`, `type`, `spec` etc. to the list of keywords. While the grammar might not be ambiguous now, it might become ambiguous later, and I'd like to avoid a design deadlock where adding e.g. `type` as a keyword isn't possible without breaking user code.
  - Clear all `TODO (syntax)` entries.
- Add `case` expressions and pattern matching in anonymous function parameters, variable declarations, and the left-hand side of assignments (e.g. for assigning tuple values to mutable variables).
  - Clear all `TODO (case)` entries.
- Refactor Pyramid and add more types and functions.
  - Clear all `TODO (pyramid)` entries.

##### MVL improvements

- Implement iterators as a first-class feature with the keyword `iter`. Iterators should be able to be passed around as values or to be wrapped in another type (such as a Sequence or Stream type), which would also be able to wrap lists and maps. Iterators should also be able to be inlined, for example when directly used in a `for` loop. We'll have to see how to reconcile the concepts of a `yield` in an iterator and a `yield` in a for/while. Maybe we can combine these concepts somehow.
  - An alternative would be a template/macro system, but this would only cover the inline uses of an iterator. Maybe the inline iterator and an iterator as a value are two different concepts which Lore needs both.
  - Also implement some form of ranges for index iteration using `for`.
  - The goal should be to have a sort of "Enum" interface against which general sequence functions can be written.
  - Are transducers applicable here? (I think they're quite hard to put into a language with a static type system, but we should explore this idea more.)
- Implement a new backend for lists.
  - Clear all `TODO (lists)` entries.
- Add immutable (hash) sets with a syntax `#[A]`.
- Provide an easy way to update immutable structs and shapes. For example, Scala's case class `copy` function.
- Allow inherited shape type properties to reference declared types placed lower in the resolution order. The reasoning for this is simple: Struct properties are immune to the resolution order, because they are resolved in a second step. This allows structs to include each other as properties. Inherited shape types essentially specify the properties of a trait, so they should enjoy the same privileges. There is nothing but complexity that keeps us from realizing the resolution of inherited shape types in a second step.
  - The VM already handles this correctly.
- Add "global specialization"/"trait implementation" for tuples, lists, maps, shapes, traits, and structs.
  - This will allow us to type lists, for example, as Enums, and so on.
- Possibly add protocols. (Also see the specification proposal.)
  - This will allow us to add equality, ordering, hashing, and stringification protocols to the core, which makes the operations associated with these protocols more type-safe. For example, right now, it is possible to compare values of any two types, even those that are incomparable (e.g. `function == struct`).
  - Protocols might be mergeable with "global specialization".
- Allow non-equality (`!=`) to be implemented separately, as non-equality can sometimes be proven more quickly than equality.
- Introduce a `Number` type that supertypes both `Int` and `Real` (possibly just `type Number = Int | Real`) and that can be used for arithmetic operations. The exact semantics of such a type have to be figured out, but the easiest would be implicit conversions from `Int | Real` to `Real`, which could be supported by amending the `IntToReal` instruction such that it's idempotent if `Real` values are passed to it.
  - The motivation for a `Number` type would be defining math functions, for example, but this requires specialization instead of implicit conversion. For example, a function `func max[N <: Number](a: N, b: N): N = if a > b then a else b` would need to be specialized for `Int` and `Real`, or alternatively work with an implementation of `>` that delegates to the correct instruction based on `N`. And then there is performance, as type parameters are notoriously slow to handle in Lore's multiple dispatch. So this would require a separate compile-time specialization mechanism, which perhaps isn't worth the effort given that we only have two number types.

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
- We may need to remove open property types from struct types in run-time type variable assignments. A motivating example would be `lore.List.concat`. We probably don't want the resulting list type to contain any open properties, provided that one of the type arguments contains a struct type with open property types. Reproducing this is probably a bit harder than one might expect, because most list types are set at compile time. But we could for example create a list via a function `func single(element: A): [A] where A = [element]`. This might type the run-time list with the element's actual type, thereby leaking a possible struct's open property types into the list. The solution would be to strip any open properties from struct types inside run-time type variable assignments.
  - We have to think carefully about this, though. There might be instances where we **want** the type argument to have its open property types.
  - I think the VM already does this. Need to verify.

##### CLI

- *Currently no TODOs.*

##### Error Reporting

- Add positions to CompilationExceptions.
- Add names to errors (similar to Typescript) so that programmers can quickly google/search for Lore errors. We might also add links to a specific error documentation (or wiki) on the web to keep compiler error messages short, but still allow beginners to read an extended version of the error, along with examples (DOs and DONTs). 
- Transformation phase: If the expression of a variable declaration is incorrect, the variable won't be registered and there will be follow-up errors that may be confusing for a user. There is already code to handle a similar case if the type required of the expression is false. However, the `visitUnary` of the visitor isn't even called when the subtree expression produces compilation errors, so we will have to introduce some other mechanism to the visitor.

##### Correctness

- We might want to "degrade" the abstractness and totality constraints to warnings. The type system is so complex that the compiler cannot understand all occurrences of abstract types. Sometimes types are not really abstract, but practically/pragmatically abstract through convention. (Such as `Metal[Price]` in `schemas/goods.lore`.) In these cases, we shouldn't force the programmer to define a function that they want to keep abstract. This is rather in the spirit of dynamic languages, but would enrich Lore, I think.
  - There might also come a time when the **totality constraint** cannot be fully verified for a particularly complex multi-function. For example, when we have to cycle through an exponentially growing number of concrete subtype combinations, the compiler might choke, making the program virtually uncompilable. We might need to implement upper bounds on the number of subtypes tested.


#### Testing

- We should ideally invest in a system that can test the parts that are replicated in both the compiler and the VM with the same values. This system should read type relationships from text files and then execute tests. This is crucial because as we discover type system bugs, we should add test cases that cover those bugs.
  - Idea: The system can be implemented on the compiler side. It would have two parts: (1) immediately executing the typing tests with the compiler subtyping, equality, and fit functions. (2) Compiling the typing tests to poem binaries and using the runtime subtyping, equality, and fit functions. This would allow us to reuse the existing type parser even for the runtime tests and also allow us to parse the custom test format using fastparse. 
  - Such a system is only advisable once we have the compiler written in Lore. Otherwise we're duplicating work which doesn't directly go towards building the compiler in Lore.
  - The system has a low priority in general because there are so many other areas to improve first.


#### Code Quality

##### Architecture

- Move errors to a more central location. It should be easy to see which errors a given phase can produce. This may also allow us to merge some errors.

##### Terminology

- Rename "sum types" to "union types" for better name duality with "intersection types".
- Rename Node to SyntaxNode. DeclNode --> DeclarationSyntaxNode, TypeExprNode --> TypeSyntaxNode, etc.

##### Clean-Up

- Clean most TODOs within the source code or add them to this TODO list.


#### Editor Support

##### Language Server

- Fix the language server given the recent changes.
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
