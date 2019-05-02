# Records, Entities and Components



### Records

A **record** is a nominal data type which defines a set of properties from an associated constructor.

```
record R(val a: A, b: B, mut c: C) {
  val d: D = ...
  mut e: E = ...
  def f: F = ...
}
val r = R(a, b, c)
r.a
// r.b <-- illegal!
```

The **constructor** of a record `A` can be accessed via the same name: `A(...)`. Each parameter of the constructor is not treated as a property of the record (i.e. it won't be accessible as a property outside the record) unless it is declared as `val` or `mut`. So for example, a constructor `R(val a: A, b: B, mut c: C)` would define the properties `a` and `c`, although `b` would be available to all definitions inside `R` (private to the record).

The body of the record is used to define additional **properties**. A property is either a **definition** (denoted `def`) or a **value** (denoted `val` or `mut`). A property declared as `mut` is mutable. A definition is recomputed every time the property is accessed, while a value is computed only once. While definitions could also be implemented by multi-functions, they provide the ability to define a property about the data that is invariant and can't be changed through type specialisation.

Properties are **accessed** via the dot notation. See `r.a` in the example above. Although the dot notation is overloaded for multi-function invocation, property access takes precedence. In such a case, you can always invoke the multi-function without using the dot-notation.

###### Example

```
record Position(val x: Real, val y: Real, val z: Real) {
  val isOrigin: Boolean = x == y && y == z && z == 0.0
}
```

##### Inheritance

A record `A` may **inherit** from another record `B`. This allows `A` to inherit and override all properties `B` defines and also puts `A` into a subtyping relation `A < B`.

```
record A extends B
```

##### Ad-hoc Envelope Types

Lore supports [envelope types](types.md) already, but to make "type all the things!" particularly easy, Lore allows you to **create ad-hoc open envelope types when defining records:**

```
record Position(val x: Real as XCoord, val y: Real as YCoord, val z: Real as ZCoord)
```

Looks stupid? Wait until you accidentally pass an x-coordinate as a y-coordinate in C++.

Each envelope type becomes part of the namespace of the record, so the code above is in effect like declaring the following:

```
namespace Position {
  envelope XCoord(Real)
  envelope YCoord(Real)
  envelope ZCoord(Real)
}
```

However, the ad-hoc definition has the additional advantage that **envelope types are constructed internally**. Take the following example:

```
record Account(id: Int as Id, name: String as Name, score: Real as Score)
val jeremy = Account(1, "Jeremy", 15.37)
> jeremy.id : Account.Id
> jeremy.name : Account.Name
> jeremy.score : Account.Score
```

As you can see, the constructor takes the underlying values as arguments and doesn't require any envelope boilerplate.
