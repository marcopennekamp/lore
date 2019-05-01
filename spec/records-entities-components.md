# Records, Entities and Components



### Records

A **record** is a nominal data type which defines a set of properties from an associated constructor.

The **constructor** of a record `A` can be accessed via the same name: `A(...)`. Each parameter of the constructor is not treated as a property of the record (i.e. it won't be accessible as a property outside the record) unless it is declared as `val`. So for example, a constructor `R(val a: A, b: B, val c: C)` would define the properties `a: A` and `c: C`.

The body of the record is used to define **properties**. A property is either a **definition** (denoted `def`) or a **value** (denoted `val` or `mut`). A definition is recomputed every time the property is accessed, while a value is computed only once. While definitions could also be implemented by multi-functions, they provide the ability to define a property about the data that is invariant and can't be changed through type specialisation.

###### Examples

```
record Position(val x: Real, val y: Real, val z: Real) {
  val isOrigin: Boolean = x == y && y == z && z == 0.0
}
```

