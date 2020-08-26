# Structs, Components and Traits

**Structs, components and traits** are the mechanisms with which complex data types can be built in Lore. These three concepts can be roughly differentiated as such:

- **Structs** are data type definitions that can stand on their own or back a trait. A struct is always a subtype of all the traits that it implements and also carries this information at run-time. However, unless a trait is attached at run-time (which will be possible under certain circumstances), the traits that are attached to the instance of a struct are mostly determined at compile-time.
- **Components** are special, immutable struct properties. They are seen as an integral part of the data structure they are owned by. The type of an **entity** (a trait or struct that owns at least one component) is partly determined by the actual value of the component, which is also replicated at run-time. As such, as opposed to traits which are (mostly) determined at compile-time, the component portion of an entity's type is determined at compile-time. This has profound implications in the context of multiple dispatch.
- **Traits** are abstractions of structure and behavior. They are Lore's solution to the questions of data type abstraction, inheritance, and interfaces. Traits cannot be instantiated directly: they have to be backed by a struct.



### Structs

A **struct** describes the type and representation of user-defined data. In concrete terms, a struct is a set of **properties** (mutable and immutable) and possibly **components**, both allowing optional default values. A struct can **implement** any number of traits, which together determine the functions that the struct must implement and the components that the struct must contain. An instance of a struct, also called an object, can be **constructed** using one of two provided syntaxes. As structs describe actual instances, a struct type is **always concrete and never abstract**.

Structs do not support **inheritance** in and of themselves. Traits are the mechanism to facilitate inheritance, which has the huge advantage of cleanly separating the concerns of data representation (structs) and abstract structure/behavior (traits).

Properties (including components) can be **delimited** using commas and newlines. Both styles are permitted interchangeably, giving the ability to use both in the same struct definition, and should be chosen based on readability. Properties are **accessed** using the dot notation.

###### Syntax Example

```
struct Point { x: Real = 0, y: Real = 0, z: Real = 0 }
struct Position { mut point: Point }
struct Person {
  name: String
  age: Int = 20
  calling: String = 'Arts and Science'
  component Position
}
```

##### Construction

Creating new struct instances is possible with two independent **constructor** syntaxes. The **call syntax** is a convenient way to create instances, but with the requirement that all properties need to be specified, including those that could have a default value. In contrast, the **map syntax** is most convenient when default values should be applied to properties or when more self-evident code is desired. It is also useful when properties should be specified out of order for some pragmatic reason. 

When using the map syntax, one may **omit some verbosity** in a definition like `property: value` if the value is a variable and named exactly like the property. An example of this is included below.

Apart from these two constructor styles, all **derivative constructors** will have to be defined as ordinary (multi-)functions. 

###### Syntax Example

```
action test() {
  // Call syntax.
  let point = Point(0.5, 1.5, 2.5)
  let position = Position(point)
  // Map syntax.
  let person = Person { name: 'Mellow', Position: position }
}

action test2() { 
  // If the variable name matches the property name, it's possible to
  // omit the colon entirely.
  let name = 'Shallow'
  let person = Person { name, Position: Position(Point { }) }
}
```

##### Implementing Traits

A struct can **implement** any number of traits. This will make the struct a **subtype** of each of its traits. Since a struct is always a concrete type, it will also have to **implement all abstract functions** declared over the trait. This is implicitly handled by the properties governing abstract functions and doesn't need to be handled specially for structs.

###### Syntax Example

```
trait Hashable
function hash(object: Hashable): Int

struct Person implements Hashable { name: String }
function hash(person: Person): Int = /* Compute the hash... */
```

##### Components

TODO?



### Components





### Traits

A **trait** is a type that describes structure and behavior. In concrete terms, a trait is an abstract type that can be associated with functions defining both its **structure** and **behavior** (either abstract, concrete, or both). Since traits must be backed by an object (created from a struct), meaning the trait itself cannot be instantiated, a trait is **abstract**.

Traits can also **inherit** from (multiple) other traits and even **extend component types**. A trait `A` inheriting from a trait `B` will make `A` a strict and direct subtype of `B` and give the programmer the opportunity to specialize any functions declared over `B` for the type `A`. When extending a component type `+C`, the trait effectively declares that any struct implementing the trait must have a component of type `C`. Such traits are also called **entities**.

###### Syntax Example

```
trait T extends S1, S2, +C
```

##### Traits and Structure

Using a trait to abstract **data** is as simple as defining the right (abstract) functions. This is technically not a special language feature (at least not yet), because we are simply using existing features such as multi-functions and multiple dispatch, so an example will suffice. We could define a trait `Position` that declares the following abstract functions:

```
trait Position
function x(pos: Position): Real
function y(pos: Position): Real
function z(pos: Position): Real
```

This allows us to define various structs implementing the same `Position`, for example:

```
struct Point implements Position { x: Real, y: Real, z: Real }
struct Box implements Position { 
  xStart: Real, xEnd: Real
  yStart: Real, yEnd: Real
  zStart: Real, zEnd: Real
}
```

Of course, we also have to implement the abstract functions declared by `Position` for each of the structs implementing the trait:

```
function x(point: Point): Real = point.x
function y(point: Point): Real = point.y
function z(point: Point): Real = point.z

// The position of a box is its center!
function x(box: Box): Real = box.xStart + width(box) / 2
function y(box: Box): Real = box.yStart + height(box) / 2
function z(box: Box): Real = box.zStart + depth(box) / 2
```

You might think this will become untenable with a global namespace. You would be mostly right, although multiple dispatch still does a lot of heavy lifting even in the disambiguation department. We will introduce namespacing, likely in the form of modules, as an update to the language shortly (in terms of update order, not necessarily time) after the minimum viable language has been completed. Properly namespaced properties are a big motivator for us to introduce a module system early.

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
  println(distance(box, point)) // 3.4641...
}
```

Adding inheritance to this example would, perhaps, allow us to model positions of different dimensions:

```
trait Position2D
function x(pos: Position2D): Real
function y(pos: Position2D): Real

trait Position3D extends Position2D
function z(pos: Position3D): Real
```

Any struct implementing `Position3D` will have to provide a definition for all three of these abstract functions.

In the future, we might introduce **syntactic sugar** for the simpler forms of data abstraction, especially so that implementing data-heavy traits with a struct isn't ultra tedious. For now, we want to keep it simple though, and the idea of multi-functions once again proves to be powerful enough to get there.

##### Traits and Behavior





##### Traits as Label Types





##### Owned-By 

**TODO:** What about owned-by in traits?



### Post-MVL Extensions (Ideas)

- Easy **getters and setters** for struct properties?

- **Visibility declarations** like private/public/protected for struct properties?

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
  struct Point implements Position {
    x: Real implements Position.x
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

- One step further: Automatic, optional **memoization of properties**.

- The lack of struct inheritance currently has the big disadvantage that one cannot **"mix in" property definitions**. If you have a trait `Entity` that requires all its implementors to specify a `name: String` property and a `Sprite` component, this has to be re-declared inside every struct that implements `Entity`. I can see two main ways to solve this problem: (1) add mixins as a language feature or (2) allow users to solve this problem with a macro system. **Mixins** would firmly concern themselves with the realm of data representation; they would not even define their own types. Hence, adding mixins would preserve our stated goal of separating data representation and abstract data structure and behavior. You could declare a mixin alongside a trait if close data coupling is desired, but each struct would at least have to declare that it's using the mixin. There would be no language-level coupling between mixins and traits.

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
  
- Every entity defines a **list of components** which can be filtered and iterated over.

- **Component life cycle functions:** We can add an action `onAttached(c: C, e: +C)` (in the Lore namespace) that is called when a component `C` has been attached to an entity `+C`. Its default implementation would be for the type `Any, +Any` and simply do nothing, so it would then be possible to specialize the function for any kind of component type, without the *need* to do so.

  ```
  action onAttached(component: Any, entity: +Any) {
    // Do nothing.
  }
  
  action onAttached(a: A, entity: +A) {
    // A has been attached to an arbitrary entity.
  }
  
  action onAttached(a: A, entity: +A & +B) {
    // One special case for any entity that also has a B component.
  }
  
  action onAttached(a: A, entity: SomeEntity) {
    // Another implementation that requires the entity to be of some
    // concrete entity type.
  }
  ```

  We need the **first parameter** since we want to associate `onAttached` with one specific component. If we didn't have this first parameter, we'd essentially define `onAttached` for multiple components, such as `entity: +A & +B`. Without the first parameter, there is no way to differentiate whether this `onAttached` should belong to A or B.