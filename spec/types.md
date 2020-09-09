# Types

The **type system** is central not only to safety and correctness of programs, but also their execution at run-time through multiple dispatch. Hence, this is the first topic we will cover in this specification.

Lore has various **type constructors** with which complex types can be built, as well as **named types** and **declared types**. Named types refer to any type that can be referenced via a name, such as a type variable or trait, while declared types refer to those types that are specifically declared by the programmer—traits and structs.

**Note:** Type aliases are missing from the current specification and will be added in a later revision of the document and language.



### Syntax of Type Expressions

A **type expression** is a representation of a particular type, built with the toolbox of named types and type constructors. Their syntax can be described as such:

- `id` — A **named type** (basic type, declared type, type variable) is accessible via its name. It has to be declared somewhere before it can be used in a type expression.
- `t1 | t2 | t3` — A **sum type** is simply constructed by connecting different type expressions with `|`.
-  `t1 & t2 & t3` — An **intersection type** is constructed using the `&` symbol.
- `(t1, t2, t3)` — **Product types** describing tuple values.
- `t1 -> t2` — **Map types** describing *immutable* maps.
- `[t]` — **List types** describing *immutable* lists.
- `+C` — A **component type**. Note that `C` always has to be a *declared type*. Currently, it cannot even be a type variable. We still have to evaluate the viability of components being declared using type variables. At this point, it is simply not feasible due to how component overriding and naming is handled.

Note that the compiler immediately performs the following **simplifications** on sum and intersection types:

1. Any part of a **sum type** that is a *subtype* of another part is filtered out. Example: `Dog | Animal` becomes `Animal`, because `Dog` is trivially an `Animal`.
2. Any part of an **intersection type** that is a *supertype* of part is filtered out. Example: `Dog & Animal` becomes `Dog`, because `Dog` is trivially an `Animal`.

Type constructors have the following **precedence** (lowest priority first):

```
|                     // sum types
&                     // intersection types
->                    // map types
() (,) [] + (...) id  // unit, product, list, component, enclosed, names
```



### Basic Types

A **basic type** is one of the following, built-in *named types:* 

- `Any` is the supertype of all types, including itself.
- `Nothing` is the subtype of all types, including itself.
- `Real` represents real number values.
- `Int` represents integer values.
- `Boolean` represents boolean values.
- `String` represents string values.



### Type Variables





### Abstractness

Each type is either **abstract** or concrete. Functions may only be declared abstract if their input type has at least one abstract parameter. Since this has important implications for the general use of abstraction patterns in Lore, it is important to understand when types are abstract. Here is a list:

- A **sum type** is always abstract. (In their normal form.)
- An **intersection type** is abstract if at least one of its parts is abstract.
  - Note that there are special rules concerning traits as **augmentations**, defined further below.
- A **product type** is abstract if at least one of its elements is abstract.
- A **list type** is always concrete.
- A **map type** is always concrete.
- A **component type** is abstract if its underlying declared type is abstract.
- A **trait** is always abstract on its own and as an augmentation only abstract if it's intersecting merely with abstract types.
  - An **augmentation** trait takes diminished preference compared to a struct or any other concrete type. This is to avoid the following scenario: Assume that augmentations *could* be abstract. Define an abstract function `f(v: Struct & Trait)` over a struct `Struct` that gets called with a dynamically specialized type. That is, we create an object of type `Struct`, attach the label `Trait`, and call the abstract function. It won't be able to dispatch to specializing functions, as the struct is quite literally the end of the line (assuming no other specializing functions), and there is no implementation to be found. So `Struct & Trait` obviously shouldn't be an abstract type.
  - The "special" case in which **multiple traits intersect** is simply handled by the fact that the resulting intersection type is abstract since it does not contain a concrete type.
- A **struct** is always concrete.

