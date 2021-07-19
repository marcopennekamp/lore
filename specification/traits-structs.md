# Traits and Structs

**Traits and structs** are the abstractions with which complex data types can be built in Lore. These two concepts can be roughly differentiated as such:

- **Traits** are abstractions of hierarchy and behavior. They are Lore's solution to the questions of data type abstraction, inheritance, and interfaces. Traits cannot be instantiated directly: they have to be backed by a struct.
- **Structs** are data type definitions that can stand on their own, possibly extending some number of traits. A struct is always a subtype of all the traits that it extends and also carries this information at run-time.

Both traits and structs have a set of **common features**, including inheritance and type parameters.



### Traits

A **trait** is an abstract type that can be extended by other traits and structs, creating a subtyping hierarchy. Traits are used for the abstraction of structure and behavior. They can have **members** in the form of inherited shape properties.

###### Syntax Example

```
trait T extends A, B, { property: C }
```



### Structs

A **struct** is a set of **properties** with optional default values and optional mutability. As structs describe actual instances, a struct type is **always concrete and never abstract**. A struct may extend traits and shapes, which is elaborated on further below.

Properties can be **delimited** using commas and newlines. Both styles are permitted interchangeably, giving the ability to use both in the same struct definition, and should be chosen based on readability. Properties can be accessed using the member access notation `struct.property`.

###### Example

```
struct Point { x: Real = 0, y: Real = 0, z: Real = 0 }
struct Position { mut point: Point }
struct Person {
  name: String
  age: Int = 20
  calling: String = 'Arts and Science'
  position: Position
}
```

##### Construction

Creating new struct instances is possible with two independent **constructor** syntaxes: 

- The **call syntax** is a convenient way to create instances, but with the requirement that all properties need to be specified, including those that could have a default value. The call-syntax constructor is a **function value** that may be passed around.
- In contrast, the **map syntax** is most convenient when default values should be applied to properties or when more self-evident code is desired. It is also useful when properties should be specified out of order. When using the map syntax, one may **omit some verbosity** in a definition like `property = value` if the value is a variable and named exactly like the property. An example of this is included below.

Apart from these two constructor styles, all **derivative constructors** have to be defined as ordinary (multi-)functions. Lore doesn't support the kind of constructors that you might know from object-oriented languages.

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
struct Soldier {
  open weapon: Weapon
}
```



### Inheritance

Traits and structs can **inherit** from any number of traits and shapes. A trait or struct `A` inheriting from a trait or shape `B` will induce the strict subtyping relationship `A < B`.

**Structs cannot be extended**. Traits are the mechanism for building type hierarchies, which has the huge advantage of cleanly separating the concerns of data representation (structs) and abstract structure/behavior (traits, shapes).

If a trait or struct inherits from a shape directly or indirectly, all properties declared in the shape will become members of the trait or struct. The combined properties of all extended shape types are called the **inherited shape type**. This type definitely specifies all properties of a trait. A struct, in contrast, treats the inherited shape type as a contract: it must specify all properties contained in the inherited shape type as struct properties, with compatible types.

Since a struct is always a concrete type, **all abstract functions** of the traits a struct extends will have to be implemented for the struct. This is implicitly handled by the constraints governing abstract functions and doesn't need to be handled specially for structs.

###### Example

```
trait Hashable
function hash(hashable: Hashable): Int

struct Person extends Hashable { name: String }
function hash(person: Person): Int = /* Compute the hash... */
```



### Type Parameters

A trait or struct may be parameterized over any number of **type variables**. The presence of a type variable effectively turns the trait or struct into a *family of types*, called a **schema**. This has additional implications, especially for run-time type semantics.

Type parameters are **declared** as such, the syntax mirroring multi-function type parameter declarations:

```
trait X[A, B <: A, C >: A]
```

At the point of **construction**, type parameters must be either inferred or manually specified. The **run-time instantiation** of a type parameter is determined at compile-time, unless the type parameter is **open**. Let's consider option types as a non-trivial example:

```
trait Option[A]
struct Some[A] extends Option[A] { value: A }
struct None extends Option[Nothing]

let v1 = Some(32)        // --> Some[Int]
let v2 = Some[Real](32)  // --> Some[Real]
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
struct Some[+A] extends Option[A] { value: A }
struct None extends Option[Nothing]

let opt: Option[Real] = Some[Int](32)
```

##### Open Type Variables

Type variable instantiations are **fixed at compile-time**. This is what we want most of the time. For example, a mutable array type `Array[A]` should not vary based on its run-time contents. Some structs may not associate a type variable with a property, either, so the type argument can only be inferred at compile-time.

There are cases in which we want the type variable to be instantiated **based on the run-time type of a property**. `Option[A]` is such a case. We want to be able to specialize functions on options without unpacking the options. Consider the following code:

```
function process(option: Option[Animal]): String = 'Maybe Animal.'
function process(option: Option[Fox]): String = 'Maybe Fox.'

let animal: Animal = Fox()
process(Some(animal))
```

If `A` is fixed at compile-time, `process(Some(animal))` will evaluate to "Maybe Animal." This goes against intuition. A Some contains a single, immutable value. By all accounts, it should be possible for multiple dispatch to take the run-time type of the value into account.

**Open type variables** fill exactly this niche. In similar spirit to open properties, an open type variable is populated at run-time with the actual type of a given value. This feature is very powerful and like all good comic book heroes (and villains), it comes with a few limitations:

- Open type variables can only be part of **structs**. They make no sense in traits, because traits aren't instantiated directly.
- Open type variables must be **uniquely deducible**. This means that the type variable may only occur in one position of a single property.
- Open type variables must be **covariant**. This is easy to see: if we have a type `Some[A]` with `A` being open, we could have a variable of type `Option[Animal]` at compile time, but a value of `Some[Fox]` at run time. If `A` was invariant, we could not put this value into the variable, because `Some[Fox]` would not be a subtype of `Option[Animal]`.
- Open type variables **cannot be manually specified** in square brackets at the point of instantiation, and must be omitted if other static type variables should be manually specified.
- Properties typed with an open type variable must be **immutable**.

The option examples demonstrates how open type variables can be **declared:**

```
trait Option[+A]
struct Some[open +A] extends Option[A] { value: A }
struct None extends Option[Nothing]
```



### Usage

This section contains **examples** on the usage of traits and structs. These aren't strictly language features, but still so fundamental to idiomatic Lore that they are worth elaborating on.

##### Data Abstraction

Using a trait to create **data abstractions** is as simple as defining the right (abstract) functions. This is not a special language feature, because we are using existing features such as multi-functions and multiple dispatch. We could define a trait `Position` that declares the following abstract functions:

```
trait Position
function x(pos: Position): Real
function y(pos: Position): Real
function z(pos: Position): Real
```

This allows us to define various structs extending the same `Position`, for example:

```
struct Point extends Position { x: Real, y: Real, z: Real }

struct Box extends Position { 
  xStart: Real, xEnd: Real
  yStart: Real, yEnd: Real
  zStart: Real, zEnd: Real
}
```

Of course, we also have to implement the abstract functions declared by `Position` for each of the structs extending the trait:

```
function x(point: Point): Real = point.x
function y(point: Point): Real = point.y
function z(point: Point): Real = point.z

// The position of a box is its center!
function x(box: Box): Real = box.xStart + width(box) / 2
function y(box: Box): Real = box.yStart + height(box) / 2
function z(box: Box): Real = box.zStart + depth(box) / 2
```

Finally, we could declare a function that just works with the data provided by `Position`: 

```
function distance(pos1: Position, pos2: Position): Real = {
  let dx = x(pos2) - x(pos1)
  let dy = y(pos2) - y(pos1)
  let dz = z(pos2) - z(pos1)
  sqrt(dx * dx + dy * dy + dz * dz)
}

action test() {
  let box = Box(0, 10, 0, 10, 0, 10)
  let point = Point(3, 7, 9)
  println(distance(box, point)) // --> 3.4641...
}
```

Adding inheritance to this example would allow us to model positions of different dimensions:

```
trait Position2D
function x(pos: Position2D): Real
function y(pos: Position2D): Real

trait Position3D extends Position2D
function z(pos: Position3D): Real
```

Any struct extending `Position3D` will have to provide a definition for all three of these abstract functions.

In the future, we might introduce **syntactic sugar** for the simpler forms of data abstraction, especially so that implementing data-heavy traits with a struct isn't ultra tedious. For now, we want to keep it simple though, and the idea of multi-functions once again proves to be powerful enough to get there.

##### Behavioral Abstraction

Traits are natural abstractions for **behavior**. Just as with data abstraction, multiple dispatch provides the ability to work with abstract and concrete functions. To Lore, a trait itself is just an "empty" type. All the magic happens within the multi-functions.

As a simple example of behavior abstractions, consider a trait `Hashable` that requires its implementors to provide a `hash` function:

```
trait Hashable
function hash(value: Hashable): Int
```

Consider we have a trait `Statistic` that should be hashable, so that we can use stats as keys in a map:

```
trait Statistic extends Hashable
function uniqueName(statistic: Statistic): String
```

Instead of implementing the hash function for every statistic individually, we can implement it just for the trait, relying on the unique name supplied by concrete statistics:

```
function hash(statistic: Statistic): Int = hash(uniqueName(statistic))
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
action hit(monster: Monster) { ... }
action hit(monster: Monster & Dead) {
  // Do something else if the monster is dead.
}
```

Right now, it is not possible to attach a label type to a value at run-time, so label types can only be "attached" by having a struct extend the label type. But once we introduce **dynamic specialization and generalization**, label types will be attachable to and removable from existing values, provided their compile-time types still agree. Then it becomes a matter of moving labels traditionally handled as object properties to the type space and harnessing the power of multiple dispatch. For example, one could attach their own label type to values that are declared in a library, then specialize some library functions for types that also have the label.



### TODOs

- **Map syntax alternative:**

  ```
  %{ name: 'Mellow', position } as Person
  Person(%{ name: 'Mellow', position })  // This clashes with the idea that Person is a unique function value.
  %Person{ name: 'Mellow', position }
  ```

  This allows us to work with the established shape syntax instead of having two parallel syntaxes.

- Easy **getters and setters** for struct properties?

- **Visibility declarations** like private/public/protected for struct properties?

  - It'd probably be best to keep visibility in the module system. I don't see many advantages in building firewalls for data definitions. If a struct desperately needs to hide some data, it should hide behind a trait abstraction.

- **Syntactic Sugar for Properties:**

  ```
  trait Position
  property x: Real of Position
  
  action test(pos: Position) {
    println(pos.x)
  }
  ```

  This is internally still a multi-function definition. Here is the general syntax:

  ```
  property name: Type of Trait
  --> function name(self: Trait): Type
  
  property mut name: Type of Trait
  --> function name(self: Trait): Type
  --> function setName(self: Trait, value: Type): ()
  ```

  And then implement it like this:

  ```
  // Direct mapping
  struct Point extends Position {
    x: Real extends Position.x
  }
  
  // Indirect mapping
  property x: Real of box: Box = box.xStart + width(box) / 2
  ```

  This could also be used as syntactic sugar for derived properties:

  ```
  property width: Real of box: Box = box.xEnd - box.xStart
  property x: Real of box: Box = box.xStart + box.width / 2
  ```

  The `property` syntax thus wouldn't only be allowed for traits but for any types. We could define properties over tuples:

  ```
  property first: A of tuple: (A, B) where A, B = get(tuple, 0)
  ```

  Note: Since this proposal does not add any additional expressiveness to the language (only convenience), it is not a candidate for the MVL itself. Also, another question is how this system interacts with namespacing, and thus it would be prudent to define the module system first and THEN turn our attention to this proposal.

  - **Alternative:**

    ```
    trait Position {
      x: Real
    }
    
    action test(pos: Position) {
      println(pos.x)
    }
    
    // Direct mapping
    struct Point extends Position {
      x: Real extends Position.x
    }
    
    // Indirect mapping
    property x: Real of box: Box = box.xStart + width(box) / 2
    // or:
    function x(box: Box): Real = box.xStart + width(box) / 2
    // or:
    property x(box: Box): Real = box.xStart + width(box) / 2
    ```

    This would internally still be represented by multi-functions, but the declaration in traits is shorter and more natural. **Potential downside:** This way of declaring trait properties could confuse users into thinking that properties are inherited. It could also make the idea that trait properties are just multi-functions under the hood harder to convey. Another question is which namespace/module these property functions will be part of.

    Another downside is that we can't easily tie properties to ANY types with this syntax, which would be especially problematic for computed properties of structs. However, we could consider supporting both syntaxes. In fact, the trait property syntax would be the natural way for traits (at the trait's declaration site, of course, not for "monkey patching"), while the `property` syntax would be the natural way for other types. Going a step further: `property` would be the keyword for functions that accept a single `instance` parameter and are called like `instance.property` *without* parentheses. (What about setters, then?)

- One step further: Automatic, optional **memoization of properties**.

- The lack of struct inheritance currently has the big disadvantage that one cannot **"mix in" property definitions**. If you have a trait `Entity` that requires all its implementors to specify `name: String` and `sprite: Sprite` properties, this has to be re-declared inside every struct that extends `Entity`. I can see two main ways to solve this problem: (1) add mixins as a language feature or (2) allow users to solve this problem with a macro system. **Mixins** would firmly concern themselves with the realm of data representation; they would not even define their own types. Hence, adding mixins would preserve our stated goal of separating data representation and abstract data structure and behavior. You could declare a mixin alongside a trait if close data coupling is desired, but each struct would at least have to declare that it's using the mixin. There would be no language-level coupling between mixins and traits.

  - A way to implement mixins would be **mixing in shape types**.

    ```
    struct Position { mut x: Real, mut y: Real }
    type +Position = { position: Position }
    
    struct Player {
      mix +Position
    }
    ```

- **Companion namespaces** for any declared type. (See also the next proposal in this list.)

- **Ad-hoc envelope types:** Lore will support envelope types. To make "type all the things!" particularly easy, Lore allows you to **create ad-hoc open envelope types when defining structs:**

  ```
  struct Position {
    x: Real as X
    y: Real as Y
    z: Real as Z
  }
  ```

  Each envelope type becomes part of the (companion) namespace of the struct , so the code above implicitly declares the following:

  ```
  namespace Position {
    envelope X(Real)
    envelope Y(Real)
    envelope Z(Real)
  }
  ```

  However, the ad-hoc definition has the additional advantage that **envelope types are constructed internally**. Take the following example:

  ```
  struct Account {
    id: Int as Id
    name: String as Name
    score: Real as Score
  }
  let jeremy = Account(1, "Jeremy", 15.37)
  > jeremy.id : Account.Id
  > jeremy.name : Account.Name
  > jeremy.score : Account.Score
  ```

  The constructor takes the underlying values as arguments and doesn't require any envelope boilerplate.
  
- **Attaching properties at run-time:** Adding properties to arbitrary structs and shapes could be very powerful combined with structural dispatch. (Especially to dynamically add components to a struct.)

- **Named type parameters:** If a struct has multiple type parameters, but only one needs to be inferred, the user should be able to specify only that one type parameter manually. We can add named type parameters here.

