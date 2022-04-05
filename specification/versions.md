# Language Versions

### First Language Version

The **first version** of Lore, which is still in development, includes or will include the following features:

- **Types:** Sum types, intersection types, tuple types, function types, list types, map types, shape types, symbol types, trait and struct types, type variables, abstractness, type inference, type aliases.
- **Functions:** Multi-functions, function declarations with type parameters, multiple dispatch, abstract functions, fixed functions, static "no dispatch" functions.
- **Data Types:** Traits, structs, shapes, properties, constructors, mutability, trait polymorphism.
- **Expressions:** Literals and value constructors (numbers, strings, booleans, tuples, anonymous functions, multi-function values, lists, maps, shapes, symbols, structs), basic operators, append for lists and maps, pipe operator, pattern matching (`case` expression with guards, and in parameters), blocks, (multi-)function calls, trailing lambdas, conditional expressions (`if` and `cond`), loops (`for` and `while`), property access, variable declarations, assignments, return.
- **Modules:** Module declarations, imports and name resolution, companion modules.
- **Miscellaneous:** Global constants, `domain` blocks.

The language will also be accompanied by a standard library called **Pyramid**, which will include the following features:

- **Collections:** Important (functional) operations on lists and maps.
- **IO:** *Basic* support for file system input/output.
- **Math:** Important mathematical functions, types, and constants.
- **Options:** Support for options, important functional operations on options.
- **Strings:** Important string functions.

This version of Lore will be used to **write the compiler in Lore**.


### Going Further

The following feature are planned or considered for the future:

- **Exceptions:** Lore currently doesn't have exceptions, which is especially bad when multiple dispatch fails with an empty fit or ambiguity error, because the VM will simply terminate. We also need exceptions to implement non-local returns from anonymous functions.
- **Trait/Struct extensions:** Traits and structs are quite bare bones. Other documents contain various ideas how to make traits and structs more comfortable and expressive, especially regarding properties. Some of the ideas are: mixins, virtual properties defined via multi-functions, shape subtyping that includes virtual properties, an improved "map syntax" for struct construction, and more.
- **Dynamic specialization and generalization:** The ability to attach and remove trait types to/from struct values at run time. This allows a programmer to further specialize multi-functions using intersection types (e.g. `Monster & Dead`, `Dead` having been attached at run time), but also comes with significant implementation difficulties and performance issues.
  - **Predicate dispatch** would be a more powerful alternative, but also likely harder to implement while keeping good performance. The big advantage of intersection types would also be dampened slightly by the use of predicate dispatch. On the other hand, regular multiple dispatch with dynamic specialization/generalization has to assume that any function accepting traits/structs could be affected by it, as any struct value could have a new type attached at run time. Predicate dispatch, in contrast, is more explicit and multi-functions that have predicate dispatch can be clearly distinguished from those that don't. This likely makes optimization for functions without explicit predicate dispatch easier. 
    - Predicate dispatch could be combined with intersection types if we allow "predicate types" which assert a fixed predicate given the value. For example, a predicate type `Dead` could be defined as `pred Dead(m: Monster) = m.health <= 0`. Using such a type in a multi-function would mark it clearly as using predicate dispatch. Such predicate types could additionally be used in variable declarations and property types.
- **Monadic expressions:** Expressions like Scala's monadic `for` or Haskell's `do` would make working with monads easier in Lore.
- **Multi-functions as operators:** The ability to define operators as multi-functions, such as an operator `+` for `Position` types.
- **Dispatch disambiguation:** Multiple dispatch will inevitably lead to unforeseen ambiguities at run time, especially in complex typing situations. Languages like Julia disambiguate by choosing the first or last definition of the fitting functions, but this is outside the spirit of Lore as function declarations can be split freely across files and in any order. We can implement an explicit disambiguation solution that works with priorities or some other mechanism across files and libraries. 
- **Envelope types**
- **Interoperability:** The VM and Lore itself should be able to interface with C libraries.
- **Macros:** Templates with simple AST substitution, as well as macros with compile-time execution.
