# Overview

**Lore** is a general-purpose programming language featuring multi-functions, structs and traits, a static type system with sum and intersection types, and a mix of functional and imperative programming. Take a look at it:

```
use lore.Enum.map

func hello(String | Int): String
func hello(name: String): String = 'Hello, $name.'
func hello(id: Int): String = 'Hello, anonymous #$id.'

func main(): [String] = ['world', 5, 'marco', 'console', 42] |> map(hello)
```

This is [hello_name.lore](../test/lessons/hello_name.lore), a hello world example making use of Lore's multiple dispatch. Depending on whether `hello`'s argument is a `String` or an `Int`, it either returns `'Hello, name.'` or `'Hello, anonymous #id.'`. The program results in a list of strings:

```
['Hello, world.', 'Hello, anonymous #5.', 'Hello, marco.', 'Hello, console.', 'Hello, anonymous #42.']
```

Read on below to find out what Lore can do.



### Feature Highlights

Take a look at these feature highlights:

- **Multi-functions** are first-class functions that dynamically dispatch on all their parameter types ([multiple dispatch](https://en.wikipedia.org/wiki/Multiple_dispatch)). When a multi-function is called, dynamic dispatch chooses the implementation based on *all* argument types at run time. Multi-functions provide specialization and abstraction out of the box, with the ability to define and extend functions across file and even project boundaries. The functionality provided by multi-functions would otherwise be covered by classes, mixins, extension functions, and similar language constructs. Together with subtyping, user-defined type hierarchies, and intersection types, specialization via multiple dispatch becomes an exceedingly powerful tool. Multiple dispatch is the *core paradigm* of Lore which informs all design decisions.
- **Traits** describe type hierarchy, abstract structure and behavior, while **structs** describe data layouts. This idea of structural and behavioral abstraction on the one hand and the *implementation* of that structure and behavior on the other leads to a clean separation of concerns that works well with dispatch hierarchies.
- **Sum types** such as `String | Int` express that a value might either be a `String` *or* an `Int`. They are exceedingly useful as return types, but also support a fluent style akin to dynamically typed languages. A list `[123, 'ABC', 'hello', 5]` has the type `[String | Int]`. In a statically typed language without sum types, the list's type might have been coerced to a supertype `[Value]`, losing information. If a function accepts `String | Int`, the function can be used to map over all elements in the list, without the need to disambiguate the sum type with pattern matching or type casts. This is exactly what happens in the [hello_name.lore](../test/lessons/hello_name.lore) example above.
- **Intersection types** such as `A & B` allow expressing that a value is both an `A` and a `B`. Multiple dispatch allows specializing parameter types by employing intersection types. For example, in a game, the default `move` function for an `Entity` might move the entity, while a specialized function with an `Entity & Stationary` parameter for stationary entities would avoid moving the entity.
- **Shapes** are views on a subset of a struct's properties. They enable dispatching on actual property types, such as accepting all structs with a property `name: String`. The programmer is able to pass any structs containing the required properties. Shape types can be used in intersection types. Shapes can also be instantiated in the form of shape values, which are useful for creating ad-hoc data structures, similar to dictionaries from dynamically typed programming languages.

Apart from these highlights, Lore strives to be a **modern, functional-first programming language**. Our goal is to create a beautiful language that places high importance on programmer happiness. Working with functions, type hierarchies, and data structures shouldn't be tedious but fun.



### Learn More

To learn more about Lore, take a look at the following core documents:

- [**Fragments:**](fragments.md) This document describes which top-level declarations Lore source code may contain.
- [**Types:**](types.md) Lore's static type system is central to the semantics of multiple dispatch, so almost every other feature of Lore builds on top of the type system.
- [**Multi-Functions:**](multi-functions.md) This document presents the basics of multi-functions and functions, multiple dispatch, type parameters, abstraction and specialization, and also offers some usage examples.
- [**Traits and Structs:**](traits-structs.md) Traits and structs are Lore's user-defined data types.
- [**Shapes:**](shapes.md) Shapes support multiple dispatch on properties, and the creation of ad-hoc data structures.
- [**Expressions:**](expressions.md) Lore is an expression-based language, which means that there are no statements, only expressions. This document presents the kinds of values and expressions available in Lore.
- [**Modules:**](modules.md) The module system supports splitting Lore programs into namespaces.
- [**Specs:**](specs.md) Specs allow a Lore programmer to define tests and benchmarks and to execute them via VM commands.
- [**Versions:**](versions.md) This document provides an overview of Lore's current and planned features.

You don't need to read these documents in order, but it would be beneficial to read up on types and multi-functions before the other topics. There is simply no way around the very foundation of the language.

Also consider taking a look at these **supplemental documents:**

- [**Identifiers:**](identifiers.md) The rules surrounding different kinds of identifiers in Lore.
- [**Scopes:**](scopes.md) A short overview of the different kinds of lexical scopes in Lore.
- [**Equality and Order:**](equality-order.md) The rules of default equality and order for all kinds of values in Lore, as well as guidelines for implementing custom equality and order.
- [**Run-time Types:**](runtime-types.md) Multiple dispatch requires types to be carried along even during program execution. This document describes Lore's run-time type semantics, including several important compromises which a Lore programmer should be aware of.
