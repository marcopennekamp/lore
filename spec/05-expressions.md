# Expressions

This document outlines all valid **expressions** found in Lore.

**TODO:** How do we implement type casts / conversions?



### Literals & Value Constructors

*On the first day, there was the **value**.* We cannot define complex expressions without having values first. We also consider their types, if they haven't been defined yet.

##### Numbers

Lore supports **real** and **integer** numbers. Their types are `Real` and `Int`. `Int` is a subtype of `Real`. They will both be implemented using the standard Javascript number type, and so precise integers can only be guaranteed up to [MAX_SAFE_INTEGER](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Number/MAX_SAFE_INTEGER). We believe this is sufficient for all common use cases. If larger numbers than $2^{53}-1$ are desired, we recommend using a big integer implementation.

We are not introducing integers for performance reasons. Rather, we think that it is useful to **state intent with the type:** if a function expects a `Real` number, it can be any number, while a function expecting an `Int` is explicitly requiring the number to be an integer. This leads to self-documenting code and guarantees that are useful when comparing for equality and such.


##### Strings

**Strings** are interpolated by default.

###### Examples

```
val k = 10
val p: Person = /* ... */
val announcement = "${p.name}, you have $k apples. Please claim your ${if (k < 10) "free" else "very costly"} apple at the reception."
val s = "'I'm rich! I made 1000\$ today!' he said. 'You're a moron, Peter,' Adelaide said."
```

