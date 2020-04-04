# Classes, Entities and Components

**TODO:** What about attaching components at run-time? We need that feature, but probably not for the first language version. This should be developed hand-in-hand with dynamic specialization.

**TODO:** How do we create classes that override component types? In general, class property types can't be overridden in subclasses, because properties can both be set and gotten by the parent class. But: Shouldn't classes be covariant in the components they define, as components cannot be swapped out, only set with the constructor? So if a type `Entity3D` has a component `Position3D`, shouldn't a subtype `Entity2D` be able to have a `Position2D`?

**Idea:** Add a Record data type (or maybe named case class, simple class, struct, etc.), which is akin to case classes in Scala. The user will be able to pattern-match values of this type, which is not possible with classes. A record can't have components, but will be able to become a component of some entity.

```
record Position(x: Real, y: Real, z: Real) {
  derived isOrigin: Boolean = x == y && y == z && z == 0.0
}
```






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

##### Extensions for Later

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



### Entities

**Idea:** Every entity defines a list of components which can be filtered and iterated over.

An **entity** is a class associated with one or more components. In addition to property definitions permitted in records, an entity type may also define components.

```
entity E(...) {
  component C1
  component btv: C2
  component C3 = ...
}
```

A component may be **unnamed**, in which case the type's *default naming scheme* is invoked. A component can be optionally named and may have a default value. Only one component of the same type may be part of an entity.

Entities are **instantiated** as follows, taking the definition of `E` from above:

```
val e = E(...)(c1 = C1(...), btv = C2(...))
```

That is, each entity's constructor is associated with an additional parameter list which expects the components as values.

##### Inheritance

An entity may also inherit from another entity or record. The case of **entity inheritance** is particularly interesting, since we can override component definitions:

```
entity Skeleton extends Monster {
  component PoisonImmunity overrides Immunity
  component Bones
}
```