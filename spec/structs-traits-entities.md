# Structs, Traits, and Entities

**Structs, components and traits** are the mechanisms with which complex data types can be built in Lore. These three concepts can be roughly differentiated as such:

- **Structs** are data type definitions that can stand on their own, possibly implementing some number of traits. A struct is always a subtype of all the traits that it implements and also carries this information at run-time. However, unless a trait is attached at run-time (which will be possible under certain circumstances), the traits that are attached to the instance of a struct are determined at compile-time.
- **Traits** are abstractions of structure and behavior. They are Lore's solution to the questions of data type abstraction, inheritance, and interfaces. Traits cannot be instantiated directly: they have to be backed by a struct.
- **Entities** are traits or structs that contain at least one component. **Components** are immutable properties that represent a *part* of an entity. Any entity can be viewed in part as one or multiple of its components. An entity with two components `A` and `B` can be viewed as the type `+A`, `+B`, or `+A & +B`. We could then write a function over `+A & +B`, take advantage of the fact that both components are part of the entity, and implement some sort of behavior concerning this exact structure. The type of an entity is ultimately determined at run-time from the actual types of its components. This has profound implications in the context of multiple dispatch.



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

A struct can contain any number of **components**, which are properties that must be traits or a structs (and not more complex types, or even lists) and don't allow declaring a name. Instead, the name of a component is the same as the name of its type. Only one component of the same type may be part of the struct. A component can be **accessed** like any other property. A struct implementing at least one component is also called an **entity**.

If a struct implements a **trait** extending component types, the struct will have to satisfy these by declaring the correct components. If a trait extends a component type `+C1`, the struct will have to contain a component of type `C2` given `C2 <: C1`.

###### Syntax Example

```
struct Skeleton {
  component Position
  component Health
}
```

##### Owned-By 

**TODO:** What about owned-by in structs?



### Traits

A **trait** is a type that describes structure and behavior. In concrete terms, a trait is an abstract type that can be associated with functions defining both its **structure** and **behavior** (either abstract, concrete, or both). Since traits must be backed by an object (created from a struct), meaning the trait itself cannot be instantiated, a trait is **abstract**.

Traits can also **inherit** from (multiple) other traits and even **extend component types**. A trait `A` inheriting from a trait `B` will make `A` a strict and direct subtype of `B` and give the programmer the opportunity to specialize any functions declared over `B` for the type `A`. When extending a component type `+C`, the trait effectively declares that any struct implementing the trait must have a component of type `C`. Such traits are also called **entities**. A trait extending an entity is itself an entity, because it also implicitly extends the component type that its parent trait extends.

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



### Entities

We have already seen how components can be declared and instantiated. In this section, we will take the time to further look at **entities**, and especially some constraints that are necessary to make the system work. Because both structs and traits can be entities, we have chosen to create this third section to define common features.

##### Typings

Let's say we have an entity `E` with a component `C1`. We say that the type `E` satisfies the typing `E & +C1`. The `+C1` is read as **"has C1"** and is a type *describing the entity*, not the component. For example, when we have a variable `e: +C1`, `e` is *not* C1 itself but rather the entity with a component of type `C1`. It can be accessed with `e.C1`.

At run-time, `E` might actually be a type `E & +C3` given `C3 < C1`, if a value of `C3` was assigned as a component as opposed to a value of `C1`. This has profound implications for **multiple dispatch:** Entities are dispatched based on their *actual* type and the *types of their components at run-time*.

##### Immutability

Once assigned to an entity, a component cannot be replaced or removed: **components are immutable**. Note that, while the *reference* is immutable, the component *itself* does not have to be immutable. You can, of course, still model changing state in Lore, but that change needs to be applied inside the component, not by replacing a component.

We would love to get rid of this **constraint**, but components must be immutable because they are part of the run-time type of the entity, as the actual component value is significant in multiple-dispatch. If a component's type could change during execution, so would the entity type, and this is not permitted due to our policy of requiring the types of run-time values to be immutable after the value's construction.

##### Component Type Restrictions

The freedom with which we allow subtyping of component types has a few **implications**. Say we have an entity type `E & C2` with `C1 < C2 < C3`. We could assign a value of either `C1` or `C2` to the entity. This already leads to a problem when we consider the compilation process:

```
struct E1 { component C1 }
struct E2 { component C2 }

action f(e: +C2) {
  e.C2
}
```

In the example above, assume that `C1` of `E1` is named `C1` in the resulting target code. How do we get `C1` through a property access of `C2`? This requires us to **resolve component values dynamically at run-time** if we try to access a subtypeable component `C2` through a generic component type.

Consider another problem: since `C2 < C3`, we should be able to **access** `C1` of `E1` via a variable of type `+C3`:

```
action f2(e: +C3) {
  e.C3 // If e has type E1, this should be a C1.
}
```

This is immediately useful, of course. But `C3` **wasn't even declared** in either `E1` or `E2`. This leads to the following awkwardness:

```
trait C
struct CA extends C
struct CB extends C

struct E {
  component CA
  component CB
}

action f(e: +C) {
  e.C // Well, that's a problem!
}
```

We don't know whether `e.C` refers to `CA` or `CB`. Well, in fact, it refers to *both*. But we don't know that `e` is `E` in `f`. We just know that *some* entity has a component of type `C`. This means we have **two options:**

1. Any component access **returns a list**.
2. Any two components defined in an entity **must not share a supertrait**.

The first option would make components effectively unusable. It is rarely desirable to have to always handle lists when we want to operate on components that should be standalone instances. Hence, we choose the **second option**.

If no components share a supertrait (`Any` does not count, since it is not a trait), we can **always decide** which component belongs to a given component type `+C`. The downside is that we cannot have two components sharing a supertrait, such as:

```
trait Stat
struct Strength extends Stat
struct Dexterity extends Stat

struct Hero {
  // Does not compile: Strength and Dexterity share a supertrait.
  component Strength
  component Dexterity
}
```

**TODO:** Maybe we could introduce traits which cannot be components, which would remove them from this whole restriction. I anticipate that there are a handful traits that this restriction will be very prohibitive for, e.g. Hashable, which might be applied to two structs that would otherwise be components coexisting in perfect harmony. (Perhaps even just with the syntax `owned by Nothing`. Might be super elegant. Although we have to be careful about how owned by translates to a subtrait or struct implementing a trait…)

But of course, we can always **rewrite the entity** in a way that allows us to implement the desired design. Consider the following example. We lose a little bit of flexibility, but we gain flexibility by being able to access components by their supertype. 

```
struct StatRepository {
  strength: Strength
  dexterity: Dexterity
}

struct Hero {
  component StatRepository
}
```

Luckily, by the way, **`+Any`** is not a valid type, since `Any` is not a trait or struct. So we don't shoot ourselves in the foot by disallowing the full trait hierarchy.

##### Ownership

**TODO**







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