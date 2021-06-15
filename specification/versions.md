# Language Versions

### Minimum Viable Language

At this early stage of design and development, my goal is to create a **minimum viable language** that satisfies the most important language goals. While a language can contain many quality of life features, the core ideas are what sets Lore apart from other languages and thus are most important. As I am a one-man team, my goal is to create a minimum viable specification and compiler that is just one thing: usable.

Here is a **list of features** that will make up the minimum viable language:

- **Types:** Sum types, intersection types, product types, function types, list types, map types, shape types, atom types, trait and struct types, simple parametric types in functions, abstractness, type inference, type aliases.
- **Functions:** Multi-functions, function declarations, multiple dispatch, abstract functions, compile-time constraints, fixed functions.
- **Data Types:** Traits, structs, shapes, properties, constructors, mutability, trait polymorphism.
- **Expressions:** Literals and value constructors (numbers, strings, booleans, tuples, anonymous functions, lists, maps, shapes, atoms, structs), basic operators, blocks, (multi-)function calls, multi-functions as function values, conditional expressions, loops, property access, variable declarations, assignments, and return.
  - TODO: Pipe operator?
  - TODO: Trailing lambdas?
- **Global Constants:** Declaration, usage.
- **Modules:** Module declarations, exports and imports, access operation on modules.

The language will also be accompanied by a standard library called **Pyramid**, which will have the following features:

- **Collections:** Important functional operations on lists and maps.
- **IO:** *Basic* support for file system input/output.
- **Math:** Important mathematical functions, types, and constants.
- **Options:** Support for options via sum types and atom types, important functional operations on options.
- **Strings:** Important string functions.


### Going Beyond the MVL

Once we have formulated the MVL, we can deliver **themed updates** that focus on a specific cross-cutting feature. For example, we could update with the following themes:

- The **Higher Data Types** Update (parametric types in structs and traits)
- The **Matchbox** Update (pattern matching in trailing lambdas, switch/match expression, guards)
- The **Specialization** Update (dynamic specialization, dynamic generalization, attaching and removing properties at run-time)
- The **Monads** Update (monadic collections, monadic options, generalized `for` or `do`)
- The **Refined Dispatch** Update (differentiating between dispatchable and static parameters?, multiple parameter lists?, dot notation for multi-function calls?, multi-functions as operators, preferred functions or other means of disambiguation)
- The **Structs and Traits** Update (syntactic sugar for common patterns, mixins?, companion namespaces?)
- The **Useful Types** Update (envelope types, ad-hoc envelope types)
- The **Interoperability** Update (dynamic types, duck typing for dynamic types, or gradual typing)
  - This depends on the runtime. One question is whether we'll stick with Javascript as a transpilation target.
- The **Macros** Update
