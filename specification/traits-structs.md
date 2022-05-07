# Traits and Structs

**Traits and structs** are used to create user-defined data types. These two concepts can be roughly differentiated as such:

- **Traits** are abstractions of hierarchy and behavior. They are Lore's solution to the questions of data type abstraction, inheritance, and interfaces. Traits cannot be instantiated directly: they have to be backed by a struct.
- **Structs** carry properties and can be instantiated, possibly extending some number of traits. A struct is always a subtype of all the traits that it extends.

Both traits and structs have a set of common features, including inheritance and type parameters.



### Traits

A **trait** is an abstract type that can be extended by other traits and structs, creating a subtyping hierarchy. Traits are used for the abstraction of structure and behavior. They can have **members** in the form of inherited shape properties.

###### Syntax Example

```
trait T extends A, B, %{ property: C }
```

Extending the shape type `%{ property: C }` adds a member `property: C` to `T`.



### Structs

A **struct** is a data type carrying a set of **properties** with optional default values and optional mutability. As structs describe actual instances, a struct type is always concrete and never abstract. A struct may extend traits and shapes, which is elaborated on further below.

Structs can be defined using two **syntax styles**, either with properties in parentheses directly after the name, or with properties in a block, which offers more flexibility. In the block style, properties can be delimited using commas and newlines.

Properties can be **accessed** using the member access notation `struct.property`.

###### Example

```
struct Empty()
struct Empty
end

struct Point(x: Real = 0, y: Real = 0, z: Real = 0)
struct Point
  x: Real = 0, y: Real = 0, z: Real = 0
end

struct Position(mut point: Point)
struct Position
  mut point: Point
end

struct Person
  name: String
  age: Int = 20
  calling: String = 'Arts and Science'
  position: Position
end
```

##### Construction

Creating new struct instances is possible with two independent **construction** syntaxes: 

- The **call syntax** is a convenient way to create instances, but with the requirement that all properties need to be specified, including those that could have a default value. The call-syntax constructor is a **function value** that may be passed around.
- In contrast, the **map syntax** is most convenient when default values should be applied to properties or when more self-evident code is desired. It is also useful when properties should be specified out of order. When using the map syntax, one may **omit some verbosity** in a definition like `property = value` if the value is a variable and named exactly like the property. An example of this is included below.

Apart from these two constructor styles, all **derivative constructors** have to be defined as ordinary (multi-)functions. Lore doesn't support the kind of overloaded constructors that you might know from object-oriented languages.

###### Example

```
// Call syntax.
let point = Point(0.5, 1.5, 2.5)  // Constructor type: (Real, Real, Real) => Point
let position = Position(point)    // Constructor type: Point => Position

// Map syntax.
let person = Person { name = 'Mellow', position = position }

// If the variable name matches the property name, it's possible to omit the property name entirely.
let name = 'Shallow'
let person2 = Person { name, position }
```

##### Open Properties

Normally, the **run-time type of a struct property** is not part of the run-time type of the struct. The struct will simply assume the property's compile-time type at the point of construction. However, some properties may need to be typed at run time to support structural subtyping. Such properties must be declared to be `open`. You can read more on the reasoning behind this in the document about [shapes](shapes.md).

###### Syntax Example

```
struct Soldier
  open weapon: Weapon
end
```

##### Objects

An **object** is a special struct of which only one instance exists. The corresponding value of an object `None` is also called `None`. In this sense an object is like a singleton, just with better language-level guarantees.

While objects may have properties, they each have to have a default value, which becomes the object's **property value**. This is because an object is instantiated by the Lore VM. A Lore programmer has no control over the relevant constructor invocation. Objects cannot have type parameters.

It is possible to use objects inside the property values of other objects. Objects with property values that refer to functions, structs, objects, or global variables are initialized **lazily** to avoid any ordering issues that may arise. Cyclical use is undefined behavior that will lead to run-time errors.

###### Syntax Example

```
object None extends Option[Nothing]

// Accessing an object via its name.
let option: Option[Int] = None

// Objects (e.g. None) can be used in the properties of other objects (e.g. Game). This particular example essentially 
// creates mutable global state, which should be avoided if possible. Handle mutability with care!
object Game do
  name: String = 'Match Four'
  mut player: Option[Player] = None
end
```

Note that a **block-style** object requires the keyword `do`. This is because a very important form of objects, propertyless "empty" objects like `None` in the example, can be declared without parentheses and the `end` keyword. The intuitive syntax for this common case introduces an ambiguity for block-style objects in the `end` keyword. It is resolved with the `do`.



### Inheritance

Traits and structs can **inherit** from any number of traits and shapes. A trait or struct `A` directly inheriting from a trait or shape `B` will induce the strict, direct subtyping relationship `A < B`.

**Structs cannot be extended**. Traits are the mechanism for building type hierarchies, which has the advantage of cleanly separating the concerns of data representation (structs) and abstract structure/behavior (traits, shapes).

If a trait inherits from a shape directly or indirectly, all properties declared in the shape will become (virtual) members of the trait. The combination of all extended shape types is called the **inherited shape type**. This type definitely specifies all properties of a trait. A struct, in contrast, treats the inherited shape type as a contract: it must define all properties contained in the inherited shape type as struct properties, with compatible types.

Since a struct is always a concrete type, **all abstract functions** of the traits a struct extends will have to be implemented for the struct. This is implicitly handled by the constraints governing abstract functions and doesn't need to be handled specially for structs.

###### Example

```
trait Hashable
func hash(Hashable): Int

struct Person extends Hashable
  name: String 
end
func hash(person: Person): Int = /* Compute the hash... */
```



### Type Parameters

A trait or struct may be parameterized over any number of **type variables**. The presence of a type variable effectively turns the trait or struct into a *family of types*, called a **schema**. This has additional implications, especially for run-time type semantics.

Type parameters are **declared** as such, with the syntax mirroring other type parameter declarations:

```
trait X[A, B <: A, C >: A]
```

At the point of **construction**, type parameters must be either inferred or manually specified. The **run-time instantiation** of a type parameter is determined at compile-time, unless the type parameter is **open**. Let's consider a naive version of option types as an example:

```
trait Option[A]
struct Some[A](value: A) extends Option[A]
object None extends Option[Nothing]

let v1 = Some(Fox())          // --> Some[Fox]
let v2 = Some[Animal](Fox())  // --> Some[Animal]
```

The compiler will try its best to infer all type variables from the given properties, but this is not always possible. The same applies to **constructor function values**. Consider the following example:

```
map([1, 2, 3], Some[Int])  // --> [Some[Int]]
map([1, 2, 3], Some)       // --> [Some[Int]]
```

Both of these variants should work, as the compiler has enough context to infer that `A = Int` for the second `Some`.

##### Variance 

Type parameters can be **covariant**, **contravariant**, or **invariant:**

- A **covariant** type parameter `+A` induces the following subtyping relationship: `X[A] <: X[B]` if `A <: B`.
- A **contravariant** type parameter `-A` induces the following subtyping relationship: `X[A] >: X[B]` if `A <: B`.
- An **invariant** type parameter `A` induces the following subtyping relationship: `X[A] <: X[B]` if `A = B`.

Covariant and contravariant type variables are subject to some **restrictions.** In general, covariant type variables may not appear in contravariant positions, and vice versa. A few consequences of this rule are, assuming that `A` and `B` are type variables:

- A mutable property `mut a: A` requires that `A` is invariant.
- A property `list: [A]` requires that `A` is *not* contravariant.
- A property `f: A => B` requires that `A` is *not* covariant and `B` is *not* contravariant.

For example, **options** can actually be *covariant:*

```
trait Option[+A]
struct Some[+A](value: A) extends Option[A]
struct None extends Option[Nothing]

let opt: Option[Real] = Some[Int](32)
```

##### Open Type Variables

Type variable instantiations are **fixed at compile-time**. This is what we want most of the time. For example, a mutable array type `Array[A]` should not vary based on its run-time contents. Some structs may not associate a type variable with a property, either, so the type argument can only be inferred or manually specified at compile-time.

There are cases in which we want the type variable to be instantiated **based on the run-time type of a property**. `Option[A]` is such a case. We want to be able to specialize functions on options without unpacking the options. Consider the following code:

```
func process(Option[Animal]): String = 'Maybe Animal.'
func process(Option[Fox]): String = 'Maybe Fox.'

let animal: Animal = Fox()
process(Some(animal))
```

If `A` is fixed at compile-time, `process(Some(animal))` will evaluate to `'Maybe Animal.'`. This goes against intuition. A `Some` contains a single, immutable value. By all accounts, it should be possible for multiple dispatch to take the value's run-time type into account.

**Open type variables** fill exactly this niche. In similar spirit to open properties, an open type variable is populated at run-time with the actual type of a given value. This feature is very powerful and comes with a few limitations:

- Open type variables can only be part of **structs**. They make no sense in traits, because traits aren't instantiated directly.
- Open type variables must be **covariant**. This is easy to see: if we have a type `Some[A]` with `A` being open, we could have a variable of type `Option[Animal]` at compile time, but a value of `Some[Fox]` at run time. If `A` was invariant, `Some[Fox]` would not be a subtype of `Option[Animal]`.
- Open type variables may not have a **lower bound**, because they can easily lead to run-time errors.
- Open type variables must be **uniquely deducible**. This means that the type variable may only occur in one position of a single property type.
- Properties typed with an open type variable must be **immutable**.

The option example demonstrates how open type variables can be **declared:**

```
trait Option[+A]
struct Some[open +A](value: A) extends Option[A]
struct None extends Option[Nothing]
```

Open type variables may still be **manually specified**, but their run-time type will not adhere to the specified type. For example:

```
let option = Some[Animal](Fox())  // option: Some[Animal]
```

`option` will have the type `Some[Animal]` at compile time, even though inference would usually have given it the type `Some[Fox]`. However, at run-time, since the type parameter is open, `option` will contain a value of type `Some[Fox]`.



### Usage

This section contains examples on the **usage** of traits and structs. These aren't strictly language features, but still so fundamental to idiomatic Lore that they are worth elaborating on.

##### Data Abstraction

Using a trait to define **data abstractions** is as simple as defining (abstract) functions. This is not a special language feature, because we are using existing features such as multi-functions and multiple dispatch. We could define a trait `Position` that declares the following abstract functions:

```
trait Position
func x(Position): Real
func y(Position): Real
func z(Position): Real
```

This allows us to define various structs extending the same `Position`, for example:

```
struct Point(x: Real, y: Real, z: Real) extends Position

struct Box extends Position
  x_start: Real, x_end: Real
  y_start: Real, y_end: Real
  z_start: Real, z_end: Real
end
```

Of course, we also have to implement the abstract functions declared by `Position` for each of the structs extending the trait:

```
func x(point: Point): Real = point.x
func y(point: Point): Real = point.y
func z(point: Point): Real = point.z

// The position of a box is its center!
func x(box: Box): Real = box.x_start + width(box) / 2
func y(box: Box): Real = box.y_start + height(box) / 2
func z(box: Box): Real = box.z_start + depth(box) / 2
```

Finally, we could declare a function that just works with the data provided by `Position`: 

```
func distance(pos1: Position, pos2: Position): Real = do
  let dx = x(pos2) - x(pos1)
  let dy = y(pos2) - y(pos1)
  let dz = z(pos2) - z(pos1)
  sqrt(dx * dx + dy * dy + dz * dz)
end

act test() do
  let box = Box(0, 10, 0, 10, 0, 10)
  let point = Point(3, 7, 9)
  println(distance(box, point)) // --> 3.4641...
end
```

Adding inheritance to this example would allow us to model positions of different dimensions:

```
trait Position2D
func x(Position2D): Real
func y(Position2D): Real

trait Position3D extends Position2D
func z(Position3D): Real
```

Any struct extending `Position3D` will have to provide a definition for all three of these abstract functions.

In the future, we might introduce **syntactic sugar** for the simpler forms of data abstraction, especially so that implementing data-heavy traits with a struct isn't so tedious. Virtual properties (like `x`, `y`, and `z` above) and inherited shape types currently fulfill a similar purpose, but are not compatible with each other. This is a design oversight that we want to remedy eventually.

##### Behavioral Abstraction

Traits are natural abstractions for **behavior**. Just as with data abstraction, multiple dispatch provides the ability to work with abstract and concrete functions. To Lore, a trait itself is mostly just an "empty" type. All the magic happens within the multi-functions.

As a simple example of behavior abstractions, consider a trait `Hashable` that requires its implementors to provide a `hash` function:

```
trait Hashable
func hash(Hashable): Int
```

Consider we have a trait `Statistic` that should be hashable, so that we can use stats as keys in a map:

```
trait Statistic extends Hashable
func unique_name(Statistic): String
```

Instead of implementing the hash function for every statistic individually, we can implement it just for the trait, relying on the unique name supplied by concrete statistics:

```
func hash(statistic: Statistic): Int = hash(unique_name(statistic))
```

Note that `Statistic` wouldn't need to extend the `Hashable` trait just to provide an implementation for the `hash` function. The value of having `Statistic` implement `Hashable` is chiefly twofold:

1. We can be sure that all functions required by `Hashable` are implemented for `Statistic`.
2. We can treat a `Statistic` as a `Hashable`.

##### Label Types

Conceptually, a **label type** is a type that adds some additional semantic meaning to another type it's attached to. For example, a sorted list could be represented as `[A] & Sorted` and a dead monster could be represented as `Monster & Dead`. We would declare these label types as follows:

```
trait Sorted
trait Dead
```

The core usefulness of a label type comes from the idea that we can **specialize functions** when the label is present:

```
act hit(monster: Monster) do
  ...
end

act hit(monster: Monster & Dead) do
  // Do something else if the monster is dead.
end
```

Right now, it is not possible to attach a label type to a value at run-time, so label types can only be "attached" by having a struct extend the label type. But if we introduce **dynamic specialization and generalization**, label types will be attachable to and removable from existing values, provided their compile-time types still agree. Then it becomes a matter of moving labels traditionally handled as properties to the type space and harnessing the power of multiple dispatch. For example, one could attach their own label type to values that are declared in a library, then specialize some library functions for types that also have the label.
