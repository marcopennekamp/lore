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

When using the map syntax, one may **omit some verbosity** in a definition like `property = value` if the value is a variable and named exactly like the property. An example of this is included below.

Apart from these two constructor styles, all **derivative constructors** will have to be defined as ordinary (multi-)functions. 

###### Syntax Example

```
action test() {
  // Call syntax.
  let point = Point(0.5, 1.5, 2.5)
  let position = Position(point)
  // Map syntax.
  let person = Person { name = 'Mellow', Position = position }
}

action test2() { 
  // If the variable name matches the property name, it's possible to
  // omit the colon entirely.
  let name = 'Shallow'
  let person = Person { name, Position = Position(Point { }) }
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

A struct can contain any number of **components**, which are properties that must be traits or structs (and not more complex types, even lists) and are not declared with a name. Instead, the name of a component is the same as the name of its type. Only one component of the same type may be part of the struct. A component can be **accessed** like any other property. A struct implementing at least one component is also called an **entity**.

If a struct implements a **trait** extending component types, the struct will have to satisfy these by declaring the correct components. If a trait extends a component type `+C1`, the struct will have to contain a component of type `C2` given `C2 <: C1` or just `C1` itself.

###### Syntax Example

```
struct Skeleton {
  component Position
  component Health
}
```

##### Ownership

Struct **ownership** follows the rules of entity ownership.

###### Syntax Example

```
struct MoverAI implements AI owned by +Position
```



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

Traits are natural abstractions for **behavior**. Just as with data abstraction, multiple dispatch provides the ability to work with abstract and concrete functions. To Lore, a trait itself is just an "empty" type. All the magic happens within the functions.

As a simple **example** of behavior abstractions, consider a trait `Hashable` that requires its implementors to provide a `hash` function:

```
trait Hashable
function hash(value: Hashable): Int
```

Consider we have a trait `Statistic` that should be hashable, so that we can use stats as keys in a map:

```
trait Statistic extends Hashable
function uniqueName(stat: Statistic): String
```

Instead of implementing the hash function for every stat individually, we can implement it just for the trait, relying on the unique name supplied by other stats:

```
// Assuming that hash is implemented for Strings...
function hash(stat: Statistic): Int = hash(uniqueName(stat))
```

Note that `Statistic` wouldn't need to extend the `Hashable` trait just to provide an implementation for the `hash` function. 

##### Traits as Label Types

Conceptually, a **label type** is a type attached to another type that describes the given values in some way. For example, a sorted list could be represented as `[Any] & Sorted` and a dead monster could be represented as `Monster & Dead`. We would declare these label types as follows:

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

Right now, it is not possible to attach a label type to a value at run-time, so label types can only be "attached" when a struct is instantiated. But once we introduce **dynamic specialization and generalization**, label types will be attachable to and removable from existing values, provided their compile-time types still agree. Then it becomes a matter of moving labels traditionally handled as object properties to the type space and harnessing the power of multiple dispatch. For example, one could attach their own label type to values that are declared in a library, then specialize some library functions for types that also have the label. This might lead to unprecedented levels of flexibility.

##### Ownership

Trait **ownership** follows the rules of entity ownership.

###### Syntax Example

```
trait Movable owned by +Position
```



### Entities

We have already seen how components can be declared and instantiated. In this section, we will take the time to further look at **entities**, and especially some constraints that are necessary to make the system work. Because both structs and traits can be entities, we have chosen to create this third section to define common features.

##### Typings

Let's say we have an entity `E` with a component `C1`. We say that the type `E` satisfies the typing `E & +C1`. The `+C1` is read as **"has C1"** and is a type *describing the entity*, not the component. For example, when we have a variable `e: +C1`, `e` is *not* C1 itself but rather the entity with a component of type `C1`. It can be accessed with `e.C1`.

At run-time, `E` might actually be a type `E & +C3` given `C3 < C1`, if a value of `C3` was assigned as a component as opposed to a value of `C1`. This has profound implications for **multiple dispatch:** Entities are dispatched based on their *actual* type and the *types of their components at run-time*.

##### Immutability

Once assigned to an entity, a component cannot be replaced or removed: **components are immutable**. Note that, while the *reference* is immutable, the component *itself* does not have to be immutable. You can, of course, still model changing state in Lore, but that change needs to be applied inside the component, not by replacing a component.

Right now, this limitation exists due to **implementation concerns**. Components must be immutable because they are part of the run-time type of the entity. If a component's type could change during execution, so would the entity type, and this is currently not possible. Once we introduce dynamic specialization and generalization in the future, we will also permit mutable components.

Note that one object can be a component of **multiple entities**. For example, you could share a health state between two bosses in a boss fight.

##### Component Type Restrictions

The freedom with which we allow **subtyping of component types** has a few implications. Say we have an entity type `E & C2` with `C1 < C2 < C3`. We could assign a value of either `C1` or `C2` to the component. This already leads to a problem when we consider the compilation process:

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
2. Any two components defined in a struct **must not share a supertype**.

The first option would make components effectively unusable. It is rarely desirable to have to always handle lists when we want to operate on components that should be standalone instances. Hence, we choose the **second option**.

If no components share a supertype (`Any` does not count), we can **always decide** which component belongs to a given component type `+C`. The downside is that we cannot declare two components in a struct that share a supertype, such as:

```
trait Stat
struct Strength implements Stat
struct Dexterity implements Stat

struct Hero {
  // Strength and Dexterity cannot be both components of Hero since
  // they share a supertype.
  component Strength
  component Dexterity
}
```

To help with this issue, any trait or struct can be declared to be **independent**. An independent type cannot be owned by an entity, cannot be used as the underlying type of a component type, and consequently doesn't need to be included in the component type restrictions. Any independent type can have **non-independent subtypes**, but all of the type's **supertraits must be independent**.

```
independent trait Stat
struct Strength implements Stat
struct Dexterity implements Stat

struct Hero {
  // Success!
  component Strength
  component Dexterity
}
```

We can rewrite another one of the above **examples** as such:

```
independent trait C
struct CA extends C
struct CB extends C

struct E {
  component CA
  component CB
}

// So far, so good. The entity E is now valid, because CA and CB only
// share the supertype C, which is independent.

// +C is now illegal, because C is independent! Hence, the typing cannot
// exist and the predicament of accessing e.C does not occur.
action f(e: +C) {
  e.C
}
```

Especially **common traits**, such as `Hashable`, profit from this, but also **label types**. So we could declare `independent trait Hashable` and then have two components that are hashable in the same entity. If this feature didn't exist, it would be virtually impossible to work with common traits.

**Traits**, on the other hand, can have components that share a supertype perfectly coexist:

```
trait C
trait C1 extends C
trait C2 extends C
struct C12 implements C1, C2

trait A extends +C1
trait B extends +C2
struct E implements A, B {
  component C12
}
```

The component `C12` provides a backing for both `+C1` and `+C2`. Whether `C12` is accessed through trait `A` and `.C1` or trait `B` and `.C2` does not matter. It is clear to the runtime which component must be accessed and no ambiguity presents itself in this example.

##### Ownership

Some components may **depend** on the entity that owns them. For example, an AI component may rely on a Position component, and so their owner `E` must contain such a component, signified by the typing `E <: +Position`. We want to provide a native way to deal with such dependencies.

When declaring a struct or trait, you can declare that a component of such a type must be **owned** by an entity of a specific type, putting a constraint on the owner's type:

```
struct Sprite owned by +Position { ... }
```

This constraint is *only relevant for component declarations*. It does not in any way affect the ability to create instances of the declared class. We **guarantee at compile-time** that an entity declaring a component can in fact own it; if not, we throw a compilation error. The check simply tests whether the type bounds given by the component are compatible with the entity.

An ownership restriction may be **any kind of type**. We don't guarantee that any entity can satisfy this type, but we don't place any systematic restraints on the type. In general, you will want to restrict ownership in two cases:

- You need to **access another component of the entity**. You wouldn't be able to implement this component without access to the other component.
- You want to restrict a component to a **specific entity type**. For example, you could restrict a component `Loot` to the `Monster` entity type.

Having declared an ownership constraint, a value of a type `+C` will also give you **access to the owner's types** if `C` declared ownership restrictions. For example:

```
function draw(e: +Sprite) = {
  draw(e.Sprite.image, e.Position)
}
```

Even though we have only declared the entity to have the Sprite component, as the ownership of a Sprite is restricted to entities that also have a Position component, we can be sure that `e` **also has a Position component**. Lore recognizes this and allows you to access that other component.

Ownership restrictions are passed down via **inheritance**. A subtype may keep the current restrictions or specialize them. For the sake of being explicit, an entity must re-declare the owner type of its supertypes (or specialize them). In the case of multiple-inheritance, the inheritors owner type must be a subtype of the intersection of the owner types of *all* types it's inheriting from. For example:

```
trait A owned by +C1
trait B owned by +C2
struct E implements A, B owned by +C1 & +C2
```

Ownership is **not fully type-safe at compile-time**, because the ability to specialize ownership *and* the ability to use components that are subtypes of the actually declared components essentially clash. Hence, **at compile-time**, we only check ownership for the declared component types. Additionally, **at run-time**, we check ownership each time an entity is instantiated. This makes the language more flexible and still restricts any point of failure to entity instantiation.



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

  - **Alternative:**

    ```
    trait Position {
      x: Real
    }
    
    action test(pos: Position) {
      println(pos.x)
    }
    
    // Direct mapping
    struct Point implements Position {
      x: Real implements Position.x
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
  
- **Attaching components at run-time:** Adding components to arbitrary entities, as long as ownership and component type restrictions hold, could be very powerful combined with multiple dispatch.

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