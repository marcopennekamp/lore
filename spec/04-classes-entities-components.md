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