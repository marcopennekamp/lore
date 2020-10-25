# Minimum Viable Language

At this early stage of design and development, my goal is to create a **minimum viable language** that satisfies the most important language goals. While a language can contain many quality of life features, the core ideas are what sets Lore apart from other languages and thus are most important. As I am a one-man team, my goal is to create a minimum viable specification and compiler that is just one thing: usable.

Hence, here is a **list of features** that will make up the minimum viable language; anything beyond that is fluff—important fluff, however—that might be added at a later time.

##### Language

- **Types:** Sum types, intersection types, product types, list types, map types, trait and struct types, component types, simple parametric types, abstractness, type inference.
  - We are excluding function types for now, but will add them very soon after a minimum version has been achieved.
  - Notably, we are excluding less important types such as singleton and envelope types.
- **Functions:** Multi-functions, function declarations, multiple dispatch, abstract functions, compile-time constraints, fixed functions.
- **Data Types:** Traits, structs, entities, properties, constructors, mutability, component declarations, polymorphism, ownership.
- **Expressions:** Literals and value constructors (numbers, strings, booleans, tuples, lists, maps, instantiation), basic operators, blocks, multi-function calls, conditional expressions, loops, property access, variable declarations and assignments, return.
  - Notably, we are not yet supporting the definition of anonymous functions.
- **TODO:** What about **global variables?** Or at least **global constants**…
- **TODO:** For syntax stuff, look at DSL-style languages like Gradle. Imagine if game developers could have the flexibility to define their own DSLs within Lore.
- **Not supported yet:** See the updates list below.

##### Implementation

- A **Parser** producing an **Abstract Syntax Tree**.
- **Correctness and Type Checking and Transformation** to an intermediate **Expression Tree** that contains already resolved type, target, variable, property, etc. information.
- A **Transpiler** that transforms the Expression Tree to Javascript.
- A **Typescript runtime** that handles multiple-dispatch, type and value construction, and provides crucial implementations of data structures such as lists and maps (especially fast, immutable data structures).



### Going Beyond the MVL

Once we have formulated the MVL, we can deliver **themed updates** that focus on a specific cross-cutting feature. For example, we could update with the following themes:

- The **Module** Update (modules, imports)
- The **Specialization** Update (dynamic specialization, attaching components at run-time, dynamic generalization, removing components at run-time, component life cycle functions, preferred functions or other means of disambiguation)
  - This update is quite specific but comes so early compared to other fundamental updates because these ideas are *highly experimental* and require a lot of careful consideration. Best to do so too early than too late.
- The **Closure** Update (anonymous functions, function types, variable capture, function call expressions, multi-functions as values)
- The **Option** Update (option types, option handling)
- The **Matchbox** Update (pattern matching across the board, switch/match expression)
- The **Monads** Update (monadic collections, monadic options, generalized `for` or `do`)
- The **Refined Dispatch** Update (differentiating between dispatchable and static parameters, multiple parameter lists?, dot notation for multi-function calls, multi-functions as operators)
- The **Structs and Traits** Update (syntactic sugar for common patterns, mixins, companion namespaces)
- The **Torchlight** Update (visibility for types, functions, properties, and potentially modules)
- The **Useful Types** Update (singleton types, envelope types, ad-hoc envelope types, type aliases)
- The **Interoperability** Update (dictionaries, dynamic types, duck typing for dynamic types)
  - This depends on the runtime. One question is whether we'll stick with Javascript as a transpilation target.
- The **Macros** Update

