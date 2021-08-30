# Types

In Lore, the **type system** is central not only to safety and correctness of programs, but also their execution at run-time through multiple dispatch. Hence, this is the first topic we will cover in this specification. The content in this document certainly doesn't contain everything we could cover about the type system, but it's enough from the perspective of a language user.

Lore has various **type constructors** with which complex types can be built, as well as **named types** and **declared types**. Named types refer to any type that can be referenced via a name, such as a type variable or trait, while declared types refer to those types that are specifically declared by the programmer—traits and structs.



### Syntax of Type Expressions

A **type expression** is a representation of a particular type, built with the toolbox of named types and type constructors. Their syntax can be described as such:

- `id` — A **named type** (basic type, declared type, type variable, type alias) is accessible via its name. It has to be declared or bound in an outer or the current scope before it can be used in a type expression.
  
  - The name of a type may contain the following **special characters:**
  
    ```
    +
    ```
  
    **Struct names** cannot contain these special characters because a struct's name is also the name of its constructor. **Type variable names** may not contain a `+`, because this symbol is also used to declare covariant type parameters.
  
- `t1 | t2 | t3` — A **sum type** is simply constructed by connecting different type expressions with `|`.

- `t1 & t2 & t3` — An **intersection type** is constructed using the `&` symbol.

- `(t1, t2, t3)` — **Tuple types** describing tuple values.

- `t1 => t2` — **Function types** describing function values.

- `t1 -> t2` — **Map types** describing *immutable* maps.

- `[t]` — **List types** describing *immutable* lists.

- `{ a: A, b: B }` — **Shape types** describing structs (partially) and shape values.

- `#name` — **Symbol types** describing symbol values.

Note that the compiler immediately performs the following **simplifications** on sum and intersection types:

1. Any part of a **sum type** that is a *subtype* of another part is filtered out. Example: `Dog | Animal` becomes `Animal`, because `Dog` is trivially an `Animal`.
2. Any part of an **intersection type** that is a *supertype* of part is filtered out. Example: `Dog & Animal` becomes `Dog`, because `Dog` is trivially an `Animal`.

Type constructors have the following **precedence** (lowest priority first):

```
|                                // sum types
&                                // intersection types
=>                               // function types
->                               // map types
() (,) [] %{ ... } #id (...) id  // unit, tuple, list, shape, symbol, enclosed, names
```



### Type Aliases

A **type alias** turns any type into a named type available in the global scope. They may have any number of **type parameters**. Type aliases don't carry additional semantics, which makes them **referentially transparent**.

###### Syntax Example

```
type Unit = ()
type +Position = %{ position: Position }
type Dictionary[V] = String -> V
```



### Type Variables

A **type variable** is a type that stands in for a range of possible types. An unbounded type variable can be assigned any type, including `Any` and `Nothing`. It is possible to give bounds to a variable, thereby reducing the set of types it may take on, increasing its expressiveness. Possible bounds are:

- A **lower bound** (`V >: Lower`) is the least possible type that a type variable may contain. Any type assigned to the variable must be a supertype of the lower bound.
- An **upper bound** (`V <: Upper`) is the greatest possible type that a type variable may contain. Any type assigned to the variable must be a subtype of the upper bound.

A **type variable declaration** can occur in the signature of a function, and will be applicable to traits and structs in a future version of Lore as well. Bounds are defined in the type variable declaration. Any **usage** of a type variable implicitly assumes that the type assigned to the variable is the same as all other points of use.



### Basic Types

A **basic type** is one of the following, built-in *named types:* 

- `Any` is the supertype of all types, including itself.
- `Nothing` is the subtype of all types, including itself.
- `Real` represents real number values.
- `Int` represents integer values.
- `Boolean` represents boolean values.
- `String` represents string values.



### Sum Types

A **sum type** describes values whose types are subtypes of one or more of the sum type's parts. The operator is associative and commutative.

###### Syntax Example

```
Fish | Mammal
```

Any value that has the type `Fish` or `Mammal` (or both) is also typed by `Fish | Mammal`.



### Intersection Types

An **intersection type** describes values whose types are subtypes of *all* of the intersection type's parts. The operator is associative and commutative.

###### Syntax Example

```
Fish & Mammal
```

Any value that is *both* a `Fish` *and* a `Mammal` is typed by `Fish & Mammal`.



### Tuple Types

A **tuple type** describes tuples. It consists of a list of types called its elements.

###### Syntax Example

```
('hello', 15): (String, Int)
(a, b, c): (A, B, C)          // with a: A, b: B, c: C
```



### Function Types

A **function type** describes function values (multi-functions and anonymous functions). The function type consists of an **input type** and an **output type**.

###### Syntax Example

```
odd?: Int => Boolean
flatten: [[A]] => [A]
zip: ([A], [B]) => [(A, B)]
```



### List Types

A **list type** describes immutable lists and is covariant.

###### Syntax Example

```
[String]
[(A, B, C)]
[Fish & Mammal]
```



### Map Types

A **map type** describes immutable maps.

**TODO:** Covariance since maps are immutable now?

###### Syntax Example

```
String -> Int
A -> (B, C)
String -> (Fish & Mammal)
```



### Shape Types

A **shape type** describes any struct or shape value with a given set of properties.

A shape or struct type A is the **subtype** of a shape type B if A contains all properties that B contains. The names of the properties in question must be exactly equal, while for any given property p, the type of A's p may be a subtype of the type of B's p.

###### Syntax Example

```
%{ }                            // the empty shape type matches all shape and struct values
%{ x: Real, y: Real, z: Real }  
%{ position: Position }
%{ grotesque: Fish & Mammal }
```



### Symbol Types

A **symbol type** describes a specific symbol value. Only the symbol value `#name` can inhabit a symbol type `#name`. Symbol types are compiled such that they are interned at run time.

The **purpose** of a symbol is to represent enumerated values, an error or success code, or an alternative to a value or result. For example, we can represent different color values as `#red | #green | #blue`.

###### Syntax Example

```
#red | #green | #blue
Real | #nan
(String, Int) => (#ok | #error)
```



### Declared Types

A **declared type** is any type defined by a struct or trait and hence a type that describes user-defined data structures. Declared types are simply referred to via their name.



### Abstractness

Each type is either **abstract** or concrete. Functions may only be declared as abstract if their input type has at least one abstract parameter. Since this has important implications for the general use of abstraction patterns in Lore, it is important to understand when types are abstract. Here is a list:

- A **sum type** is always abstract. (In its normal form.)
- An **intersection type** is abstract if at least one of its parts is abstract.
  - Note that there are special rules concerning **augmentations**, defined further below.
- A **tuple type** is abstract if at least one of its elements is abstract.
- A **function type** is always concrete, as one can always define the constant function.
- A **list type** is always concrete.
- A **map type** is always concrete.
- A **shape type** is always concrete on its own. It may stand as an augmentation.
  - Shape types would behave like tuple types if we could guarantee that run-time property types are always taken into account for multiple dispatch. This would require all struct properties to be open, which we do not want to support.
- A **symbol type** is always concrete.
- A **trait** is always abstract on its own. It may stand as an augmentation.
- A **struct** is usually concrete. 
  - A parameterized struct with an **open type argument** is abstract if the type argument is abstract.

##### Augmentations

An **augmentation** takes diminished preference in an intersection type compared to a non-augmenting type. Traits and shape types can stand as augmentations.

If a **shape type** stands as an augmentation, its abstractness is based on whether it further describes a trait (making it abstract), or if it can describe shape values (making it concrete).

Consider an intersection type `T = T_1 & T_2 & T_3 & ... & T_n`. The following **algorithm** is used to determine abstractness:

1. If `T` contains **shape types:** Remove all parts from `T` that aren't shape types and call the resulting intersection type `U`. If `U` has at least one part, the abstractness of `T` reduces to the abstractness of `U` via step (2). Otherwise, `T` is concrete, as it only consists of shape types and thus describes shape values.
2. If `T` contains **trait types:** Remove all parts from `T` that aren't trait types and call the resulting intersection type `U`.  If `U` has at least one part, the abstractness of `T` reduces to the abstractness of `U` via step (3). Otherwise, `T` is abstract.
3. If `T` contains **neither shape types nor trait types:** The abstractness of `T`, which contains no augments, can be determined with the general definitions of abstractness.

The **purpose** of augmentations is to avoid the following scenario: Assume that augmentations *could* be abstract. Define an abstract function `f(v: Struct & Trait)` over a struct `Struct` that gets called with a dynamically specialized type. That is, we create an object of type `Struct`, attach the label `Trait`, and call the abstract function. It won't be able to dispatch to specializing functions, as the struct is quite literally the end of the line (assuming no other specializing functions), and there is no implementation to be found. So `Struct & Trait` obviously shouldn't be an abstract type.



### TODOs

- **Naming of declared types:** The name "Declared Types" clashes with the fact that type aliases can also be "declared", globally, in a module, and so on. Maybe we should call these types "Data Types" or something else.
