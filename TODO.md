## TODO

#### Features

- Implement a minimum viable version of the VM.
- Implement a compiler backend for producing Lore VM binaries.
  - Rename `dynamic` to `intrinsic` calls.
  - Clear all `TODO (bytecode)` entries.
- Allow multiple multi-function imports with the same name from different modules and implement compile-time disambiguation of such multi-function calls.
  - This would also allow us to introduce a `list.length`-style function call syntax. It might make type inference harder, though.
- Syntax changes:
  - Change `!`, `&&` and `||` to `not`, `and`, and `or`. Especially `!` is weird with the ability to put `?` or `!` into a function name: `!equal?(a, b) || !check?!(x)` vs. `not equal?(a, b) or not check?!(x)`.
  - Implement iterators as a first-class feature with the keyword `iter`. Iterators should be able to be passed around as values or to be wrapped in another type (such as a Sequence or Stream type), which would also be able to wrap lists and maps. Iterators should also be able to be inlined, for example when directly used in a `for` loop. We'll have to see how to reconcile the concepts of a `yield` in an iterator and a `yield` in a for/while. Maybe we can combine these concepts somehow.
  - Rename `act` to `proc`? This would be in line with `func` and `iter`.
- Allow inherited shape type properties to reference declared types placed lower in the resolution order. The reasoning for this is simple: Struct properties are immune to the resolution order, because they are resolved in a second step. This allows structs to include each other as properties. Inherited shape types essentially specify the properties of a trait, so they should enjoy the same privileges. There is nothing but complexity that keeps us from realizing the resolution of inherited shape types in a second step.
  - The VM already handles this correctly.
- Implement a new backend for lists.
  - Clear all `TODO (lists)` entries.
- Fix map types and values:
  - Add clear covariance/contravariance type semantics.
  - Make maps immutable and support this in the runtime.
  - Implement a clear appends operation for maps and make them generally usable.
  - Clear all `TODO (maps)` entries.
- Add pattern matching in `case` expressions, anonymous function parameters, variable declarations, and the left-hand side of assignments (e.g. for assigning tuple values to mutable variables).
  - Clear all `TODO (case)` entries.
- Add "global specialization"/"trait implementation" for tuples, lists, maps, shapes, traits, and structs.
  - This will allow us to type lists, for example, as Enums, and so on.
- Possibly add protocols. (Also see the specification proposal.)
  - This will allow us to add equality, ordering, hashing, and stringification traits to the core, which makes the operations associated with these traits more type-safe. For example, right now, it is possible to compare values of any two types, even those that are incomparable (e.g. `function == struct`).
  - Protocols might be mergeable with "global specialization".
- Start designing Pyramid properly.
  - Clear all `TODO (pyramid)` entries.
- Add immutable (hash) sets with a syntax `#[A]`.
  - Maps and sets can't share the `#[]` syntax, because this would make an empty map and an empty set have the same syntax. Maps or sets should have a different prefix character. 
- Further syntactic changes:
  - Implicit underscore sections (e.g. `map(things, _.name)`) or an equivalent shortcut syntax.
  - Trailing lambdas.
  - Indentation-aided parsing:
    - Significant indentation is not intended to replace `do..end`, but rather aid in parsing ambiguities. In that sense, we are calling this feature "indentation-aided parsing".
    - This would resolve some issues with `do..end` inconsistencies, such as:
      - Block-style objects and modules requiring the `do` keyword due to parsing ambiguities.
      - A then-style `if` requiring the `else` to occur on the same line as the end of the "on true" top-level expression.
      - `cond` (and later `case`) requiring `do..end` for blocks, which adds serious visual noise.
      - Concrete actions require the `do` keyword to disambiguate them from abstract actions. With significant indentation, this `do` requirement would only be needed for empty actions, where indentation-aided parsing cannot rely on an indented expression. On the other hand, the `do..end` syntax is currently consistently applied to block functions and actions. We might want to keep this requirement for stylistic reasons.
  - Consider renaming the module keyword to `mod`, which has the added benefit of being on the same column as `use`. This might be visually more pleasant.
  - Clear all `TODO (syntax)` entries.
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

- *Currently no TODOs.*

##### Type System

- These concerns apply to Lore as well: https://viralinstruction.com/posts/badjulia/#the_type_system_works_poorly
  - "In Rust, the problem is not even recognizable: Any type you write can freely derive traits."
    I already saw this problem with built-in collection types, such as lists. Say some library has a trait X, but we want lists to implement this trait. That's currently not possible, but quite easy in languages with protocols. The same happens when we have a trait X in library A and a struct Y in library B. It's currently not possible to have Y implement X. It would suffice to specify this at <b>compile time</b> and this is actually quite possible. It's not much different from extending a multi-function defined in library A. We just have to give the option, add runtime support, and make it even harder to implement incremental compilation down the line.
  - A simple syntax would be `impl Trait for Type`. This would add `Trait` as a direct supertype of `Type`. Any abstract multi-functions would have to be implemented accordingly.
  - How we can accomplish this in the runtime for built-in types such as lists and maps is another question. We can, of course, keep a global map of the supertypes of lists, maps, tuples, etc., but this might heavily affect performance.
- Reintroduce a "least upper bound"/join for complex sum types for select expressions, such as list and map constructions.
- Turn map keys and values into covariant/contravariant type variables if possible.
- Support intersection and sum types in subtyping/equality unification.
- We could theoretically introduce a limited form of ambiguity analysis at compile-time: For each function `f(a: A, b: B, ...)`, get a list of possible subtypes (mostly trait subtypes) and simulate dispatch with these types. If any of the inputs result in an ambiguity, raise at least a warning.
- We may need to remove open property types from struct types in run-time type variable assignments. A motivating example would be `lore.List.concat`. We probably don't want the resulting list type to contain any open properties, provided that one of the type arguments contains a struct type with open property types. Reproducing this is probably a bit harder than one might expect, because most list types are set at compile time. But we could for example create a list via a function `func single(element: A): [A] where A = [element]`. This might type the run-time list with the element's actual type, thereby leaking a possible struct's open property types into the list. The solution would be to strip any open properties from struct types inside run-time type variable assignments.
  - We have to think carefully about this, though. There might be instances where we **want** the type argument to have its open property types.

##### CLI

- *Currently no TODOs.*

##### Error Reporting

- Add positions to CompilationExceptions.
- Add names to errors (similar to Typescript) so that programmers can quickly google/search for Lore errors. We might also add links to a specific error documentation (or wiki) on the web to keep compiler error messages short, but still allow beginners to read an extended version of the error, along with examples (DOs and DONTs). 
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

- Finish writing the `runtime-types` document.


#### Testing

- We should ideally invest in a system that can test the parts that are replicated in both the compiler and the runtime with the same values. This system should read type relationships from text files and then execute tests. This is crucial because as we discover type system bugs, we should add test cases that cover those bugs.
  - Idea: The system can be implemented on the compiler side. It would have two parts: (1) immediately executing the typing tests with the compiler subtyping, equality, and fit functions. (2) Compiling the typing tests to Javascript and using the runtime subtyping, equality, and fit functions. This would allow us to reuse the existing type parser even for the runtime tests and also allow us to parse the custom test format using fastparse. 
  - Such a system is only advisable once we have the compiler written in Lore. Otherwise we're duplicating work which doesn't directly go towards building the compiler in Lore.
- In the long run, we should build a simple testing framework written in Lore and use it to unit-test Pyramid.
- Add multiple tests that verify that we correctly handle the negative side of totality constraint verification / abstract functions. This is especially important so that changes to the totality constraint checking algorithm don't accidentally lead to illegal programs not being detected anymore. Use the Scala testing environment for this, because the functional tests are not well suited to testing negative compilation outcomes.
  - It's debatable whether this should be added to the Scala compiler, or whether we should only implement this once we have written the compiler in Lore.


#### Code Quality

##### Architecture

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

- Fix the language server given the recent module feature addition and typechecking changes.
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
