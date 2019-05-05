# Literals



##### Strings

**Strings** are interpolated by default.

###### Examples

```
val k = 10
val p: Person = /* ... */
val announcement = "${p.name}, you have $k apples. Please claim your ${if (k < 10) "free" else "very costly"} apple at the reception."
val s = "'I'm rich! I made 1000\$ today!' he said. 'You're a moron, Peter,' Adelaide said."
```

