# Overview

**Lore** is a general-purpose programming language featuring multi-functions, structs and traits, structural typing enabling component-based programming, and intersection and semantic types. We explore the combination of **experimental features** that come together to form a sum greater than its parts. These are:

- **Multi-functions** are first-class functions that dispatch on more than one parameter (multiple dispatch). They are intrinsically interesting and provide specialization and abstraction out of the box that would otherwise be baked into classes, mixins, or similar language constructs. Together with trait and struct hierarchies, shapes, and intersection types, specialization via multiple dispatch becomes an exceedingly powerful tool. Such techniques include function specialization based on semantic types, type augmentation at runtime, and writing functions against parts of structures.
- **Traits** describe type hierarchy, abstract structure and behavior, while **structs** describe data layouts. This idea of structural and behavioral abstraction on the one hand and the *implementation* of that structure and behavior on the other leads to a clean separation of concerns.
- **Shapes** can be used to view a struct from the perspective of partial data. Because shapes enable dispatching on actual property types, they can be used to support component-based programming, among other things. The programmer is able to write functions that work with *parts* of a struct and pass any structs containing the required properties as arguments. Shapes are also useful for creating ad-hoc data structures, which are similar to dictionaries from dynamically typed programming languages.

Apart from these highlights, Lore strives to be a modern, functional-first programming language. Our goal is to create a beautiful language that places high importance on **programmer happiness**. Working with lists and maps shouldn't be tedious, but rather make you smile.



### Learn more about Lore

To learn more about Lore, take a look at the following **core documents:**

- [**Fragments:**](fragments.md) This document describes which top-level declarations Lore source code may contain.
- [**Types:**](types.md) Multiple dispatch is nothing without types, and Lore is statically typed, so the type system isn't just some afterthought. This document thus presents the type system of the language.
- [**Multi-Functions:**](multi-functions.md) This document presents the basics of multi-functions and functions, multiple dispatch, type parameters, abstraction and specialization, and also some usage examples.
- [**Traits and Structs:**](traits-structs.md) Traits and structs are the central means to define and work with data in Lore.
- [**Shapes:**](shapes.md) Shapes are the means to support multiple dispatch on properties. They are also used to create ad-hoc data structures.
- [**Expressions:**](expressions.md) Lore is an expression-based language, which means that there are no statements, only expressions. This document presents the kinds of values and expressions available in Lore.
- [**Modules:**](modules.md) The module system supports splitting Lore programs into many small parts.
- [**Versions:**](versions.md) This document provides an overview of Lore's current and planned features.

You don't need to read these documents in order, but it would be beneficial to read up on types and multi-functions before you read about the other topics. There is simply no way around the very foundation of the language.

Also consider taking a look at these **supplemental documents:**

-  [**Scopes:**](scopes.md) A short overview of the different kinds of lexical scopes in Lore.



