# Traits and Structs

**Traits and structs** are the abstractions with which complex data types can be built in Lore. These two concepts can be roughly differentiated as such:

- **Traits** are abstractions of hierarchy and behavior. They are Lore's solution to the questions of data type abstraction, inheritance, and interfaces. Traits cannot be instantiated directly: they have to be backed by a struct.
- **Structs** are data type definitions that can stand on their own, possibly extending some number of traits. A struct is always a subtype of all the traits that it extends and also carries this information at run-time.



### Traits

A **trait** is a type that describes structure and behavior. In concrete terms, a trait is an abstract type that can be associated with functions defining both its **structure** and **behavior** (either abstract, concrete, or both). Since traits must be backed by a struct, meaning the trait itself cannot be instantiated, a trait is **abstract**.

Traits can also **inherit** from (multiple) other traits and even extend shape types. A trait `A` inheriting from a trait `B` will make `A` a strict and direct subtype of `B` and give the programmer the opportunity to specialize any functions declared over `B` for the type `A`. When extending a shape type `S`, the trait effectively declares that any struct extending the trait must contain all properties declared in `S`.

###### Syntax Example

```
trait T extends A, B, S
```



### Structs

A **struct** describes the type and representation of user-defined data. In concrete terms, a struct is a set of **properties** (immutable and, if you must, mutable) allowing optional default values. A struct can **extend** any number of traits, which together determine the functions that must be implemented for the struct and, together with shape types, the properties a struct must contain. An instance of a struct, also called a struct value or simply struct, can be **constructed** using one of two provided syntaxes. As structs describe actual instances, a struct type is **always concrete and never abstract**.

Structs do not support **inheritance** in and of themselves. Traits are the mechanism to facilitate inheritance, which has the huge advantage of cleanly separating the concerns of data representation (structs) and abstract structure/behavior (traits, shapes).

Properties can be **delimited** using commas and newlines. Both styles are permitted interchangeably, giving the ability to use both in the same struct definition, and should be chosen based on readability. Properties can be accessed using the dot notation `struct.property`.

###### Syntax Example

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

Apart from these two constructor styles, all **derivative constructors** will have to be defined as ordinary (multi-)functions.

###### Syntax Example

```
action test() {
  // Call syntax.
  let point = Point(0.5, 1.5, 2.5)  // Constructor type: (Real, Real, Real) => Point
  let position = Position(point)    // Constructor type: Point => Position
  // Map syntax.
  let person = Person { name = 'Mellow', position = position }
}

action test2() {
  // If the variable name matches the property name, it's possible to omit the property name entirely.
  let name = 'Shallow'
  let person = Person { name, position = Position(Point { }) }
}
```

**TODO:** Map syntax alternative:

```
%{ name: 'Mellow', position } as Person
Person(%{ name: 'Mellow', position })  // This clashes with the idea that Person is a unique function value.
%Person{ name: 'Mellow', position }
```

This allows us to work with the established shape syntax instead of having two parallel syntaxes.

##### Extending Traits

A struct can **extend** any number of traits. This will make the struct a **subtype** of each of its traits. Since a struct is always a concrete type, **all abstract functions** of the traits will have to be implemented for the struct. This is implicitly handled by the constraints governing abstract functions and doesn't need to be handled specially for structs.

A struct can also extend **shape types**. This will require the struct to declare properties in such a way that the struct subtypes the given shape type(s).

###### Syntax Example

```
trait Hashable
function hash(hashable: Hashable): Int

struct Person extends Hashable { name: String }
function hash(person: Person): Int = /* Compute the hash... */
```

##### Open Properties

Normally, the run-time type of a struct property is not part of the run-time type of the struct. The struct will simply assume its compile-time type at the point of construction. However, some properties may need to be typed at run-time to support structural subtyping. Such properties must be declared to be `open`. You can read more on the reasoning behind this in the document about [shapes](shapes.md).

###### Syntax Example

```
struct Soldier {
  open weapon: Weapon
}
```







```
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



### Post-MVL Extensions (Ideas)

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

