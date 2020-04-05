# Classes, Entities, and Components

In this document, we consider **classes, entities, and components**.

While classes are hugely important in Lore, the true focus of the language lies on **entities and components**. A component, in short, is a set of properties that is *also* part of a larger entity. It is thus not the component itself which brings the flexibility, but the **combination of multiple components** in one entity. The novelty that I believe Lore brings to the table is that we can define functions over arbitrary combinations of components. We could define a function over a combination `+Position & +HealthState` that is then usable by *all* entities that have these two components. This allows a programmer to separate and mix data in such a way that maximum flexibility can be achieved—I believe this is especially useful for game development, but could be equally useful in general purpose contexts. My job as a language designer is to give you the tools so *you* can figure out the useful contexts.

**A modest list of TODOs:**

- **TODO:** What about attaching components at run-time? We need that feature, but probably not for the first language version. This should be developed hand-in-hand with dynamic specialization.

- **TODO:** How do we create classes that override component types? In general, class property types can't be overridden in subclasses, because properties can both be set and gotten by the parent class. But: Shouldn't classes be covariant in the components they define, as components cannot be swapped out, only set with the constructor? So if a type `Entity3D` has a component `Position3D`, shouldn't a subtype `Entity2D` be able to have a `Position2D`?

- **TODO:** We can make *immutable* properties in general overridable.

- **Idea:** Add a Record data type (or maybe named case class, simple class, struct, etc.), which is akin to case classes in Scala. The user will be able to pattern-match values of this type, which is not possible with classes. A record can't have components, but will be able to become a component of some entity.

  ```
  record Position(x: Real, y: Real, z: Real) {
    derived isOrigin: Boolean = x == y && y == z && z == 0.0
  }
  ```

- **TODO:** Component subtyping leads to a diamond problem. Consider an entity with two components: `CelsiusTemperature` and `FahrenheitTemperature`. Which component is chosen if we want to assign the entity to a variable of type `+Temperature`? This hints that we need to exclude whole hierarchies of types when we consider which components can be mutually part of the same entity.

  - Going further, will we get problems with `Any`? What happens when we assign an entity to `+Any`? (Ouch.)



### Classes

A **class** is a nominal data type defining a set of properties. Class instances are called objects. The syntax is as follows:

```
class C {
  a: A
  mut b: B
}
const c = C(a, b)
```

Each class defines exactly one **constructor**. The constructor of a class `A` can be accessed as `A(...)`. Its parameters are simply all the properties in their order of declaration. For example, the declaration above would have the associated constructor `C(a: A, b: B): C`. Any other "constructors" will have to be defined using standard multi-functions. More on that later.

A **property** is either *immutable* (denoted without a keyword) or *mutable* (denoted `mut`). Only mutable properties can be modified after the object has been constructed.

Properties are **accessed** via the dot notation. See [05-expressions](05-expressions.md).

###### Example

```
class Position {
  x: Real
  y: Real
  z: Real
}
```

##### Inheritance

A class `A` may **inherit** from another class `B`. This allows `A` to inherit all properties of `B` and also puts `A` into a subtyping relation `A < B`.

```
class A extends B
```

**TODO:** How does inheritance work with constructors? How can a subclass invoke the super constructor? How can we define a constructor that calls another functional constructor?

##### Post-MVL Extensions

- **Visibility declarations** like public, private, protected, etc. Whatever we need.

- **Default values** for properties.

- **Derived properties** are properties that depend on other properties and can't be passed through the constructor. By default, a `derived` property is computed once after all non-derived properties and derived properties ordered before the given property have been initialized. You can also declare a `computed` property that is recomputed every time it is accessed. (Computed may not be the best term for this, however.)

  While derived properties could also be implemented by multi-functions, they provide the ability to define a property about the data that is **invariant** and can't be changed through function specialisation.

  ```
  class RightTriangle {
    a: Real
    b: Real
    derived c: Real = sqrt(pow(a, 2) * pow(b, 2))
  }
  ```

- **Syntactic sugar for functions**, as seen below. These functions would simply be immutable properties that hold an anonymous function. This can be useful in some specific cases, but could also lead to bad code style or confused new language users if these kinds of functions are erroneously preferred over multi-functions. Maybe we shouldn't make it easy to declare such functions.

  ```
  class C {
    function f(a: A): B = ...
    // is the same as
    f: A => B = { a => ... }
  }
  ```

- Some kind of companion object as known from Scala? Or rather **companion namespaces**? (Also see the `namespace Position` declaration in the example below.)

- **Ad-hoc envelope types:** Lore will support [envelope types](types.md). To make "type all the things!" particularly easy, Lore allows you to **create ad-hoc open envelope types when defining classes:**

  ```
  class Position {
    x: Real as XCoord
    y: Real as YCoord
    z: Real as ZCoord
  }
  ```

  Looks stupid? Wait until you accidentally pass an x-coordinate as a y-coordinate in C++.

  Each envelope type becomes part of the namespace of the class, so the code above implicitly declares the following:

  ```
  namespace Position {
    envelope XCoord(Real)
    envelope YCoord(Real)
    envelope ZCoord(Real)
  }
  ```

  However, the ad-hoc definition has the additional advantage that **envelope types are constructed internally**. Take the following example:

  ```
  class Account {
    id: Int as Id
    name: String as Name
    score: Real as Score
  }
  val jeremy = Account(1, "Jeremy", 15.37)
  > jeremy.id : Account.Id
  > jeremy.name : Account.Name
  > jeremy.score : Account.Score
  ```

  As you can see, the constructor takes the underlying values as arguments and doesn't require any envelope boilerplate.



### Entities & Components

An **entity** is a class associated with one or more components. In addition to property definitions permitted in classes, an entity type may also define components:

```
entity E {
  x: A
  component C1
  component C2
  y: B
}
```

A component *must* be a **class or envelope**. This requirement is simple when we consider that components must also be **unnamed**, because component types such as `+C1` don't carry name information. Thus, the name of a component is the same as the name of its type. Only one component of the same type may be part of an entity.

Here, the type `E` **has** a component `C1`. The component can be **accessed** like an attribute, `e.C1`, with the type name as the accessor name. 

When a component `C1` is declared like this, the type `E` satisfies the typing `E & +C1`. The `+C1` is read as **"has C1"** and is a type *describing the entity*, not the component. For example, when we have a variable `e: +C1`, `e` is not C1 itself but rather the entity with a component of type `C1`.

Once assigned to an entity, a component cannot be replaced or removed: **components are immutable**. Note that, while the *reference* is immutable, the component *itself* does not have to be immutable. You can, of course, still model changing state in Lore, but that change needs to be applied inside the component, not by replacing a component.

(**TODO:** Consider mutable components for cases where an immutable class needs to be a component? In general, it would be possible to replace components. The only time we need immutability is when we want to override a component in a subclass.)

##### Instantiation

Entities are **instantiated** like classes, with the simple addition that component declarations are also added as parameters in the order of declaration. For example, the entity declard above would have the following constructor:

```
E(x: A, c1: C1, c2: C2, y: B)
```

##### Inheritance

An entity may also inherit from another entity or class. The case of **entity inheritance** is particularly interesting, since we can override component definitions:

```
entity Skeleton extends Monster {
  component PoisonImmunity overrides Immunity
  component Bones
}

class Position2D extends Position3D {
  // redefine z as always 0
}

entity Entity2D extends Entity3D {
  component Position2D overrides Position3D
}
```

This is specifically possible because *component properties are immutable*. The parent class cannot reassign its own components, so we are free to require more specific types for sub-entities.

##### Ownership

Some components may **depend** on other components. For example, an AI component may rely on a position component. We want to provide a native way to deal with such dependencies.

When declaring a class, you can declare that the class viewed as a component must be **owned** by an entity of a specific type. This effectively puts a constraint on the type of the component's entity:

```
// Require an entity that gets displayed via a sprite to also have a position.
class Sprite owned by +Position { ... }
```

This constraint is *only relevant for component declarations*. It does not in any way affect the ability to create instances of the declared class. We **guarantee at compile-time** that an entity declaring a component can in fact own it; if not, we throw a compilation error. The check simply tests whether the type bounds given by the component are compatible with the entity.

An ownership restriction may be **any kind of type**. We don't guarantee that any entity can satisfy this type, but we don't place any systematic restraints on the type. In general, you will want to restrict ownership for two use cases:

- You need to **access another component of the entity**. You wouldn't be able to implement this component without access to the other component.
- You want to restrict a component to a **specific entity type**. For example, you could restrict a component `Loot` to the `Monster` entity type.

Having declared an ownership constraint, a variable of type `+Sprite` will also give you **access to the owner's types:**

```
function draw(e: +Sprite) = {
  draw(e.Sprite.image, e.Position)
}
```

Even though we have only declared the entity to have the Sprite component, as the ownership of a Sprite is restricted to entities that also have a Position component, we can be sure that `e` **also has a Position component**. Lore recognizes this and allows you to access that other component.

Ownership restrictions are passed down via **inheritance**. A subclass must keep the current restrictions.

- **TODO:** Can subclasses add new restrictions? I think this would lead to runtime errors… Like this:

  ```
  class A
  class A1 extends A
  class B owned by +A
  class B1 extends B owned by +A1
  
  entity E {
    component A
    component B
  }
  
  function create(): E = {
    const a = A()
    const b: B = B1()
    // At this point, b is not obviously B1, as it could have come
    // from somewher else entirely. This might be obvious to the
    // compiler, but not in the more complicated circumstances.
    E(a, b)
  }
  ```

  All checks seem to pass, but because B1 narrows the ownership restriction, B1 is *not* owned by an entity `+A1`. Allowing subclasses to narrow this restriction would be very useful, but is sadly not feasible in this way. We will need to contemplate this further to perhaps find a better solution.

Note that one object can be a component of **multiple entities**. For example, you could share a health state between two bosses in a boss fight. We don't want to remove this freedom and so Lore requires diligence of the programmer when it comes to component instantiation.

###### Example

Let's see another example:

```
class LemmingAI owned by +Position { }

function walk(entity: +LemmingAI) = {
  // Always forward! (To the right in a sidescrolling game.)
  entity.Position.translateX(1)
}
```

##### Adding Multiple Components of the Same Type

Suppose we have a `Wheel` and want to add four of them to a `Car`. You can't simply add four components of the same type. The solution would be to use some kind of wrapper type. Both of these possible representations are illegal as components: `(Wheel, Wheel, Wheel, Wheel)` and `[Wheel]`. In fact, you will need to create a new class, for example named `WheelSet`, which holds the wheels however it wants. You can then declare it as a component and access it via `e.WheelSet`.

##### Post-MVL Extensions

- Compare to property/class extensions: **Visibility**, **default** values, **derived** components (excluding computed components).

- Every entity defines a **list of components** which can be filtered and iterated over.

- **Importing Component Properties:** We could support importing properties from a component into the entity namespace. Properties x, y, and z would be available for use just as if they were directly declared within the entity.

  ```
  entity Entity {
    component Position import {x, y, z}
  }
  ```

  - One issue with this is that this syntactic sugar only applies when the **entity type is available directly**, obviously. If we define a function over `e: +Position`, we wouldn't be able to access `x` directly from `e` (`e.x`), but would still have to write `e.Position.x`. Only when we declare `e: Entity` can we write `e.x`.