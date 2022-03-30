# Overview

**Lore** is a general-purpose programming language featuring multi-functions, structs and traits, a static type system with sum and intersection types, and a mix of functional and imperative programming. Lore explores an interesting combination of experimental features:

- **Multi-functions** are first-class functions that dynamically dispatch on more than one parameter ([multiple dispatch](https://en.wikipedia.org/wiki/Multiple_dispatch)). Dynamic dispatch decides the function implementation to execute at run time based on argument types of a function call. Multiple dispatch takes all argument types into account, not just the first. Multi-functions are intrinsically interesting and provide specialization and abstraction out of the box that would otherwise be baked into classes, mixins, extension functions, and similar language constructs. Together with subtyping, user-defined type hierarchies, and intersection types, specialization via multiple dispatch becomes an exceedingly powerful tool. Multiple dispatch is the *core paradigm* of Lore which informs all design decisions.
- **Traits** describe type hierarchy, abstract structure and behavior, while **structs** describe data layouts. This idea of structural and behavioral abstraction on the one hand and the *implementation* of that structure and behavior on the other leads to a clean separation of concerns that works well with dispatch hierarchies.
- **Shapes** are views on a subset of a struct's properties. They enable dispatching on actual property types, such as accepting all structs with a property `name: String`. The programmer is able to pass any structs containing the required properties. Shapes can also be instantiated in the form of shape values, which are useful for creating ad-hoc data structures, similar to dictionaries from dynamically typed programming languages.

Apart from these highlights, Lore strives to be a **modern, functional-first programming language**. Our goal is to create a beautiful language that places high importance on programmer happiness. Working with functions, type hierarchies, and data structures shouldn't be tedious but fun.



### Learn more about Lore

To learn more about Lore, take a look at the following core documents:

- [**Fragments:**](fragments.md) This document describes which top-level declarations Lore source code may contain.
- [**Types:**](types.md) Lore's static type system is central to the semantics of multiple dispatch, so almost every other feature of Lore builds on top of the type system.
- [**Multi-Functions:**](multi-functions.md) This document presents the basics of multi-functions and functions, multiple dispatch, type parameters, abstraction and specialization, and also offers some usage examples.
- [**Traits and Structs:**](traits-structs.md) Traits and structs are Lore's user-defined data types.
- [**Shapes:**](shapes.md) Shapes support multiple dispatch on properties, and the creation of ad-hoc data structures.
- [**Expressions:**](expressions.md) Lore is an expression-based language, which means that there are no statements, only expressions. This document presents the kinds of values and expressions available in Lore.
- [**Modules:**](modules.md) The module system supports splitting Lore programs into namespaces.
- [**Versions:**](versions.md) This document provides an overview of Lore's current and planned features.

You don't need to read these documents in order, but it would be beneficial to read up on types and multi-functions before the other topics. There is simply no way around the very foundation of the language.

Also consider taking a look at these **supplemental documents:**

- [**Scopes:**](scopes.md) A short overview of the different kinds of lexical scopes in Lore.
- [**Run-time Types:**](runtime-types.md) As types direct multiple dispatch, we require types to be carried along even during program execution. This document describes the run-time type semantics, including several important compromises which a Lore programmer should be aware of.
