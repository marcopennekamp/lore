# Overview

**Lore** is a general-purpose programming language featuring multi-functions, structs and traits, a type-safe entity-component system, and intersection and semantic types. We explore the combination of **experimental features** that come together to form a sum greater than its parts. These are:

- **Multi-functions** are first-class functions that dispatch on more than one parameter (multiple dispatch). They are intrinsically interesting and provide specialization and abstraction out of the box that would otherwise be baked into classes, mixins, or similar language constructs. Together with trait hierarchies, components, and intersection types, specialization via multiple dispatch becomes an exceedingly powerful tool. Such techniques include function specialization based on semantic types, type augmentation at runtime, and writing functions against parts of structures (traits and components) instead of the structure itself.
- **Traits** describe type hierarchy, abstract structure and behavior, while **structs** describe data layouts. This idea of structural and behavioral abstraction on the one hand and the *implementation* of that structure and behavior on the other leads to a clean separation of concerns.
- **Components** are *parts* of an entity (struct or trait). An instance can be viewed as its part in the form of a single component type (or multiple with intersection types). This allows you to take the intersection type of two components, for example, and implement a multi-function for it. This multi-function would then accept any values that have both components as their parts, no matter the base type of the object or whether additional components are also present in the object. The multi-function has a special case where an object needs to have an additional component? No problem, just specialize the function that handles the two components and include the additional component. Components are great, because they allow you to program against flexible interfaces.
  - **TODO:** Replace this with shapes.

Apart from these core features, we also have concepts and ideas in mind for **modern programming features** that other languages are lacking, or features that tie especially well into Lore.



### Learn more about Lore

To learn more about Lore, **have a look** at the following documents:

- [**Types:**](types.md) Multiple dispatch is nothing without types, and Lore is statically typed, so the type system isn't just some afterthought. This document thus presents the type system of the language.
- [**Multi-Functions:**](multi-functions.md) This document presents the basics of multi-functions, functions, actions, multiple dispatch, abstraction and specialization, and also some usage examples.
- [**Structs, Traits, Shapes:**](structs-traits-shapes.md) Structs, traits, and shapes are the central features to define and work with data in Lore.
- [**Expressions:**](expressions.md) Lore is an expression-based language, which means that there are no statements, only expressions. This document presents the kinds of values and expressions available in Lore. Especially in the context of the minimum viable language, this aspect of the language is definitely the most conservative part and will be readily familiar to most programmers. Hence feel free to skip over most of the code examples.
- [**Minimum Viable Language:**](minimum-viable-language.md) Building a language is difficult, time-consuming, and also somewhat of a risk, especially considering the many experimental features that are at the core of Lore. Hence, I am implementing Lore with the approach of a minimum viable product, which I call minimum viable language. Instead of trying to get everything right at once, I am first building a core that can be slowly expanded. If you want to read about the features planned for the minimum viable language, as well as the features planned for updates *after* the initial release, consult the linked document.

You don't need to read these documents in **order**, but it would be beneficial to read up on types and multi-functions before you read about the other topics. There is simply no way around the very foundation of the language.