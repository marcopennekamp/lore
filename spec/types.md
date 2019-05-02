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



##### Envelope Types

An **envelope type** puts a value of another type "in an envelope." This means that, while the envelope can always be opened to read the letter, a letter may not be passed as an envelope unless it's put into the envelope *with the right address* first.

In concrete terms, this means that:

1. An envelope type `E` must have an underlying value type `V`.
2. If a value of type `V` is expected, `E` can be used without any conversion.
3. If a value of type `E` is expected, `V` may not be used without first constructing a value of `E` from it explicitly.
4. For convenience, any properties of the underlying value can be accessed through the envelope value directly.

An envelope type is **defined and constructed** as such:

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
```
