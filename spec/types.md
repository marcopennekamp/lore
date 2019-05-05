# Types

In this chapter, we lay out the basics of Lore's type system. We define all kinds of types. At the end, we also look at typing rules that allow us to reason about types. Note that we have not yet added type constructor precedence to the grammar presented in this chapter. This will be refined at a later date. 

- **TODO:** Add type constructor precedence rules.
- **TODO:** Add typing rules.



### The Role of Types

**TODO**



### Basic Considerations

##### Typing

Given a type $\tau$ and a value $v$, we write $v : \tau$ iff $v$ has the type $\tau$.

##### Mirror Notation

Similar to Scala, types and values should have notations that mirror each other. For example, for a tuple `(a, b, c)`, the product/tuple type should be denoted `(A, B, C)`.



### List of Types

##### Product Types

**Product types** describe corresponding tuple values. A product type is denoted `(T1, ..., Tn)` for an arbitrary number of types `n`. A tuple value is denoted `(a1, ..., an)` with `ai : Ti` for all `1 <= i <= n`.

###### Examples

```
val t1: (String, String, Int) = ("Hello", "World", "v2")
val t2: ((Int, Int), Real) = ((1, 2), 5.44)
```



##### Envelope Types

An **envelope type** puts a value of another type "in an envelope." This means that, while the envelope can always be opened to read the letter, a letter may not be passed as an envelope unless it's put into the envelope *with the right address* first.

In concrete terms, this means that:

1. An envelope type `E` must have an underlying value type `V`.
2. If a value of type `V` is expected, `E` can be used without any conversion.
3. If a value of type `E` is expected, `V` may not be used without first constructing a value of `E` from it explicitly.
4. For convenience, any properties of the underlying value can be accessed through the envelope value directly.

An envelope type is **defined and constructed** as such (note that the type and value notations mirror each other):

```
envelope E(V)
val v: V
val e = E(v)
```

Envelope types have two important **advantages:**

- They provide a convenient way to **document** your code without writing comments. For example, take a function `score: String => Real`. With envelope types, this could be `score: Player.Name => Player.Score `. Much clearer, right?
- They improve **correctness resilience** through type checking. Since you can't pass the underlying value without constructing an envelope, you will not accidentally pass any value that doesn't (theoretically) belong there. For example, take the `score` function from above. You will have a hard time passing your grandma's name (who certainly doesn't play games!) or even another envelope type such as `Location.Name`.

###### Examples

```
envelope PhoneNumber(String)
envelope Players(List[Player])
envelope Height(Real)
```

Alternative syntax: `type Real as Height`

###### Envelope Types are not Type Aliases

A **type alias** is simply a new name for a right-hand type expression. In most programming languages, this type alias is *not* treated as a new type. This enables documentation purposes, but fails at providing correctness. We wanted to take it a step further.



##### Proposition Types

A **proposition type** models a statement `A => Boolean` over a value of type `A`. The type `A` is called the proposition's **target**. A proposition type must be attached to its target type via an intersection.

A value `v : A` may only be described by a proposition `P` if `P(v)` is `true`. Thus, the proposition type tells us something about the value as an efficient encoding in the type. This is particularly useful for multiple dispatch in conjunction with dynamic type specialisation.

A proposition type is **defined** as such, where `expr` evaluates to a `Boolean`:

```
proposition P = (a: A) => expr
```

###### Dependent Propositions (Idea)

A **dependent proposition** defines a set of propositions over some value `a`. For example:

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
proposition Dead = (e: +Health) => e.health.isDepleted
```

Note that this proposition is an assertion over the *entity* and not the component's type itself. Interpreting the context, this means that the entity is dead, *not* the health component.



### Type Properties

##### Default Naming Scheme

In some instances, for example when accessing elements of a tuple or when accessing the component of an entity, a name is required which is not readily available. To facilitate this, for every named type Lore defines a **default naming scheme** that names the property.

We have the following rules, given a type `T` to name:

1. If `T` is a record or envelope type, take the lowerCamelCase name of `T`.
2. If `T` is a component type `+A`, take the name `hasA`.
3. If `T` is an intersection type `T_1 & T_2 & ...`, name `T` according to the following rules:
   1. If `T_i` shall not be named, ignore it.
   2. Choose any `T_i` based on the following precedence: Record types = envelope types = intersection types > component types. Only choose `T_i` with the highest precedence.
   3. If one `T_i` has been chosen, determine the name of that type. Otherwise (either none or too many types have been chosen), don't name `T`.
4. The following kinds of types shall not be named: Product types, function types, sum types, proposition types, label types. One may attempt to name some of these (such as naming each part of a sum type and concatenating it together with "Or"), but the point is to provide a *simple* algorithm for *default* cases.

Additional rules affect the naming of tuple elements:

1. If any derived name appears multiple times in the tuple, these names are enumerated starting from 1. For example, `t1`, `t2`, etc.
2. If an element at position `i` can not be named, it shall be referred to as `_i`.

###### Example

```
val stuff: (Skeleton & Dead, +Health, Skeleton & Minotaur, (Player & Alive) & (+Health | +Wealth))
stuff.skeleton
stuff.hasHealth
stuff._3 // Can't derive a name from Skeleton & Minotaur, since both are entities.
stuff.player
```

Obviously this is a completely fabricated example, but you can see how the algorithm works.