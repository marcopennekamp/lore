# Types

In this chapter, we lay out the basics of Lore's type system. We define all kinds of types. At the end, we also look at typing rules that allow us to reason about types. Note that we have not yet added type constructor precedence to the grammar presented in this chapter. This will be refined at a later date. 

- **TODO:** Add type constructor precedence rules.
- **TODO:** Add typing rules.



### The Role of Types

**TODO**



### Basic Definitions

##### Typing

Given a type $\tau$ and a value $v$, we write $v : \tau$ iff $v$ has the type $\tau$.



### List of Types



##### Proposition

A **proposition type** models a statement `A => Boolean` over a value of type `A`. The type `A` is called the proposition's **target**. A proposition type must be attached to its target type via an intersection.

A value `v : A` may only be described by a proposition `P` if `P(v)` is `true`. Thus, the proposition type tells us something about the value as an efficient encoding in the type. This is particularly useful for multiple dispatch in conjunction with dynamic type specialisation.

**Idea:** A **dependent proposition** defines a set of propositions over some value `a`. For example:

```
proposition MaxSize[T](size: Int) = (list: List[T]) => list.size <= size
```

###### Examples

```
proposition Sorted[T] = (list: List[T]) => list.isSorted
```

Here, the proposition `Sorted` models an assertion `forall T. List[T] => Boolean` which states that the list is sorted. You can use this proposition to specialise a function for a list which is already sorted.

We can also define a proposition over a type parameter:

```
proposition Sorted[A[T] <: Iterable[T], T] = (it: A[T]) => it.isSorted
```

And of course, propositions can also be defined over component types:

```
proposition Dead[+Health] = (e: +Health) => e.health.isDead
```

