# Equality and Order
 
Equality and order of values in Lore is governed by three core functions, as well as the VM's default implementations for these functions. The operators `==` and `!=` are reduced to a function `lore.core.equal?`, `<` and `>` to `lore.core.less_than?`, and `<=` and `>=` to `lore.core.less_than_equal?`. Each of these functions is backed by an intrinsic, a default implementation provided by the VM, that calls back into the core functions for recursive comparisons.

This document specifies the default implementations and provides guidelines for custom implementations.



### Default Equality

- **Ints:** Two integers are equal if their bits are equal.
- **Reals:** Two reals are equal in accordance to the IEEE 754 standard for 64-bit floats.
- **Booleans:** `true` is equal to `true`, `false` is equal to `false`.
- **Strings:** Two strings are equal if they have exactly the same bytes.
- **Symbols:** Two symbols are equal if they have the same name.
- **Tuples:** Two tuples are equal if they have the same size and their elements are equal under `lore.core.equal?`.
- **Functions:** Two functions are equal if they have the same reference.
- **Lists:** Two lists are equal if they have the same length and all their elements are equal under `lore.core.equal?`.
- **Shapes:** Two shapes are equal if they have the same property names and their properties are equal under `lore.core.equal?`.
- **Structs:** Two structs are equal if they have the same schema and their property values are equal under `lore.core.equal?`.
  - This default implementation considers two structs with different open property types as equal if their property values are equal under `lore.core.equal?`, as struct types have no bearing on the definition of equality.

In comparisons between `Real` and `Int`, both the compiler and the default implementation of equality promote the `Int` to a `Real`.



### Default Order

A particular issue with ordering arbitrary values is that some values are simply incomparable, which results in partial orders. Comparison sorting algorithms require total orders, so it's desirable to ensure that most orders are total. Lore tries to give a sensible default order to any kind of value to attempt exactly this. One such measure is assigning an order to booleans, namely `false < true`.

If two values have different type kinds, the values are ordered based on the kind, in this order: `Int/Real < Boolean < String < Symbol < Tuple < Function < List < Map < Shape < Struct`. Note that in comparisons between `Real` and `Int`, both the compiler and the default implementation of order promote the `Int` to a `Real`. This kind order is instrumental in achieving a total order between arbitrary values.

- **Ints:** Two integers are ordered in accordance to standard signed 64-bit integer comparison.
- **Reals:** Two numbers are ordered in accordance to the IEEE 754 standard for 64-bit floats.
- **Booleans:** `false` is less than `true`.
- **Strings:** Strings are ordered lexicographically by code point. This ordering will not produce good user-facing results, but constitutes a sensible default. User-friendly string ordering must take locale into account, which is beyond the scope of a default implementation.
- **Symbols:** A symbol `a` is less than a symbol `b` if `a.name < b.name`.
- **Tuples:** A tuple `a` is less than another tuple `b` if `a` has fewer elements, or if `a` and `b` have the same size and their elements follow lexicographic ordering under `lore.core.less_than?`.
- **Functions:** Functions are unordered.
- **Lists:** A list `a` is less than another list `b` if their elements follow lexicographic ordering under `lore.core.less_than?`. If all elements are equal, but `a` has fewer elements than `b`, `a` is smaller.
- **Shapes:** A shape `a` is less than a shape `b` if `a` and `b` have the same property names and their properties in name order follow lexicographic order under `lore.core.less_than?`, or if `a` and `b` have the same number of properties and the property names of `a` as a list of strings are less than the property names of `b`, or if `a` has fewer properties than `b`.
- **Structs:** A struct `a` is less than a struct `b` if `a` and `b` have the same schema and their properties in declaration order are lexicographically ordered under `lore.core.less_than?`, or if `a` and `b` have different schemas and the struct type name of `a` is less than the struct type name of `b`.

Note that these defaults, especially around structs and shapes, can result in programmer errors because of a lack of compiler intervention when comparing usually incompatible types. For example, one might, by accident, compare a `Vector2` to a `Point2` without defining a `less_than?` for this combination. In such a case, an arbitrary `Point2` would always be less than `Vector2`, because of the name comparison `'Point2' < 'Vector2'` (the default for structs of different types). In the future, Lore might restrict equality and order only to types that implement an Eq/Ord trait or protocol, which would improve compile-time detection of these errors. Until then, you are encouraged to take care when defining and using comparisons of complex types.



### Custom Equality and Order

The equality and order functions in `lore.core` can be overwritten to implement custom comparisons of arbitrary pairs of types. The only type combinations that do not admit custom comparisons are `Int/Int, Int/Real, Real/Int, Real/Real, Boolean/Boolean, String/String, Symbol/Symbol`. For example, it is technically possible to override the default implementation for generic list equality, but obviously not encouraged at all.

Consider the following example struct:

```
struct Adventurer
  id: Int
  name: String
  equipment: [Item]
end
```

Two adventurers with the same ID are considered to be equal, so we don't need to check the other fields. However, for sorting adventurers, we want to order them by name and then ID, ignoring the equipment. This can be achieved with the following custom implementations:

```
module lore.core do
  func equal?(adv1: Adventurer, adv2: Adventurer): Boolean = adv1.id == adv2.id

  func less_than?(adv1: Adventurer, adv2: Adventurer): Boolean = do
    adv1.name < adv2.name || adv1.name == adv2.name && adv1.id < adv2.id
  end
end
```



### Alternative definitions for `less_than_equal?`

This is the default implementation for `less_than_equal?`: 

```
func less_than_equal?(a: Any, b: Any): Boolean = less_than?(a, b) || equal?(a, b)
```

The reason for this definition is that it preserves the identity `(a <= b) == (a < b || a == b)`. Under this view, `<=` is just syntactic sugar for the combination of `<` and `==`. 

There are legitimate reasons to provide an alternative definition for `<=`:

- **Lexicographic ordering:** A programmer may desire to define `<=` lexicographically. For example, a comparison `Struct(#ok, 'abcd') <= Struct(#ok, 'abc')` returns `false`. However, when following an alternative lexicographic definition of `<=`, the first property would be compared first, leading to a comparison `#ok <= #ok`. This is true, so the overall comparison would result in `true`. 
- **Performance:** The simple definition of `less_than_equal?` calls into `less_than?` and `equal?` separately. In most cases, it's faster to combine these two concerns in a specialized function. Providing an alternative implementation for performance-critical types may be a valid path.
