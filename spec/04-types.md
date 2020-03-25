# Types

In this chapter, we lay out the basics of Lore's type system. We define all kinds of types. At the end, we also look at typing rules that allow us to reason about types. Note that we have not yet added type constructor precedence to the grammar presented in this chapter. This will be refined at a later date.

- **TODO:** Add type constructor precedence rules.
- **TODO:** Add all typing rules (especially subtyping rules).
- **TODO:** This should probably be placed before multi-functions.md, because we are defining a lot of stuff here that is used in multi-functions.md.



### The Role of Types

**TODO**



### Conceptual Considerations

##### Mirror Notation

Similar to Scala, types and values should have notations that mirror each other. For example, for a tuple `(a, b, c)`, the product/tuple type should be denoted `(A, B, C)`.



### Definitions

##### Type Statements

Given a type $t$ and a value $v$ of that type, we write $v : t$ to denote that $v$ has the **type** $t$.

Given a type $t$ and a subtype $s$, we write $s \leq t$ to denote that $s$ is a **subtype** of $t$. 

Given a type $t$ and a subtype $s$ with $t \neq s$, we write $s < t$ to denote that $s$ is a **strict subtype** of $t$.


##### Declared Types

Some types in Lore have to be defined via **declaration** instead of by means of construction (intersection types, for example, are constructed types). Declared types are **class and label types**.

Declared types are inherently placed in a **hierarchy**. A declared type may declare its **supertype**. For a declared type $t$, we construct the set $\mathrm{sub}_{D_1}(t)$ of **direct declared (strict) subtypes** as follows: For all types $s$ whose supertype is declared to be $t$, we have $s \in \mathrm{sub}_{D_1}(t)$.

Additionally, **we define:**

- A function $\mathrm{is\_declared}(t)$ that is true iff $t$ is a **declared type**.

- A **set of declared types** $\mathbb{T}_D = \{ t \in \mathbb{T} \mid \mathrm{is\_declared}(t) \}$.

- A set $\mathrm{sub}_D(t)$ which contains all **declared (strict) subtypes** of $t$:
  $$
  \mathrm{sub}_D(t) = \mathrm{sub}_{D_1}(t) \cup \bigcup_{t' \in \mathrm{sub}_{D_1}(t)} \{ s \mid s \in \mathrm{sub}_D(t') \}
  $$

- Given a declared type $t$ and a subtype $s$, we write $s <_1 t$ to denote that $s$ is a **direct declared subtype** of $t$ and define:
  $$
  s <_1 t \iff s \in \mathrm{sub}_{D_1}(t)
  $$

##### Values and Defined Values

Conceptually, some types $t$ have a set of **values** $\mathrm{val}(t)$ and a set of **own values** $\mathrm{ownval}(t)$. (TODO: Rename ownval to $\mathrm{val}_\mathrm{own}$?)
$$
\mathrm{val}(t) = \{ v \mid v : t \} \\
\mathrm{ownval}(t) = \{ v \mid v : t \land [\forall s < t. \neg (v : s)] \}
$$
That is, $\mathrm{val}$ describes the set of all values that inhabit the given type, while $\mathrm{ownval}$ describes the set of all values that inhabit the given type but not any of its subtypes (that also have ownvals).

It doesn't make sense to define these sets for all types. In general, defining $\texttt{ownval}$ is **useful for types that describe their own values**. These are: Class types, product types, function types (although thinking about function ownvals is largely useless), component types, and singleton types.

The following types **do not define an ownval set**:

- **Intersection types and sum types** are classic *semantic type constructors* that serve a strict type-theoretic purpose. We do not need to think of them in terms of value sets. As they only "limit" or "aggregate" values, they don't define values themselves.
- Likewise, **label types** might not be type constructors, but they are *semantic types* that merely augment existing types with additional information for the type system; they do not "touch" values in any way and thus don't define values themselves.

##### Abstract Types

In general, a type $t$ is **abstract** if and only if $\mathrm{ownval}(t) = \empty$. That is, a type is abstract if all values that inhabit the type also inhabit one of its subtypes, and thus it doesn't define any values itself. Each kind of type has its own criteria for abstractness, which will be supplied and proven below.

Some types **don't have an $\mathtt{ownval}$ set**. In these cases, we define their abstractness over some other kind of type property, most likely in an algebraic manner.



### Typing Rules

For now, we are only defining subtyping rules here.

##### Declared Types

$$
\frac{\mathrm{is\_declared}(s) \and \mathrm{is\_declared}(t) \and s \in \mathrm{sub}_D(t)}{s \leq t}
$$
Note that the current compiler implements this rule for labels and classes separately. If we unify the hierarchy in a tree, we can implement this rule for declared types in general.

##### Intersection Types

$$
\frac{\forall t' \in \{ t_1, \dots, t_m \}. [\exists s' \in \{ s_1, \dots, s_n \}. s' \leq t' ]}{(s_1 \texttt{ & } \dots \texttt{ & } s_n) \leq (t_1 \texttt{ & } \dots \texttt{ & } t_m)}
$$

If $n = 1$ or $m = 1$, we have special cases in the implementation, but the rule above also covers those cases. In case of $n = 1$, we determine whether any type $s$ is a subtype of intersection type $t$ by checking that $s$ be the subtype of *all* components of $t$. In case of $m = 1$, we determine whether any type $t$ is the supertype of intersection type $s$ by checking that $t$ is the supertype of *any* component of $s$.

##### Sum Types

$$
\frac{\forall s' \in \{ s_1, \dots, s_n \}. [\exists t' \in \{ t_1, \dots, t_m \}. s' \leq t']}{(s_1 \mid \dots \mid s_n) \leq (t_1 \mid \dots \mid t_m) }
$$

As with intersection types, we implement special cases for $n = 1$ and $m = 1$, but they are also covered by the rule above.

##### Product Types

$$
\frac{\forall i \in \{ 1, \dots, n \}. s_i \leq t_i}{(s_1, \dots, s_n) \leq (t_1, \dots, t_n)}
$$

##### Any Type

$$
\frac{}{s \leq \mathrm{Any}}
$$



### List of Types

##### Product Types

**Product types** describe corresponding tuple values. A product type is denoted `(T1, ..., Tn)` for some arbitrary number of types $n \geq 1$. A tuple value is denoted `(a1, ..., an)` with $a_i : T_i$ for all $1 \leq i \leq n$. Any value or type at any position is called a **component** of the tuple.

###### Own Values

**Property 1:** $\mathrm{ownval}(T) = \mathrm{ownval}(A_1) \times \dots \times \mathrm{ownval}(A_n)$ given $T = (A_1, \dots, A_n)$.

**Proof:** $\mathrm{ownval}(T) \subseteq \mathrm{ownval}(A_1) \times \dots \times \mathrm{ownval}(A_n)$

Let $t = (a_1, \dots, a_n) \in \mathrm{ownval}(T)$. Assume there is an $a_i \notin \mathrm{ownval}(A_i)$. Hence, there is a subtype $S_i < A_i$ for which $a_i $ inhabits $S_i$, since $a_i : A_i$ will still need to hold. For $t$ to be an own value of $T$, it must hold for all $S < T$ that $t$ does not inhabit $S$. However, if $a_i$ inhabits $S_i$, $t$ inhabits a type $S = (S_1, \dots, S_n) < T$ with $S_j = A_j$ for all $j \neq i$. Thus, $t$ cannot be an own value of $T$, proving the statement by contradiction.

**Proof:** $\mathrm{ownval}(T) \supseteq \mathrm{ownval}(A_1) \times \dots \times \mathrm{ownval}(A_n)$

Let $t = (a_1, \dots, a_n) \in \mathrm{ownval}(A_1) \times \dots \times \mathrm{ownval}(A_n)$ and $t : T$. Assume there is such a $t$ for which $t \notin \mathrm{ownval}(T)$. Since $t$ inhabits $T$, there would have to be a subtype $S < T$ for which $t \in \mathrm{ownval}(S)$, with $S_i \leq A_i$ and $S_j < A_j$ for at least one $j$. We know that $\mathrm{ownval}(S) \subseteq \mathrm{ownval}(S_1) \times \dots \times \mathrm{ownval}(S_n)$ and hence $a_j \in \mathrm{ownval}(S_j)$. But this contradicts the definition of $t$ whereby $a_j \in \mathrm{ownval}(A_j)$, since own value sets are necessarily disjunct. We prove that tuple $t$ as defined must be an own value of T.

###### Abstractness

A product type is abstract if and only if **any of its component types are abstract**.

**Proof:** *If any component type is abstract then the product type is abstract.*

Let $T = (A_1, ..., A_n)$ with at least one abstract type $A_i$. For the purposes of a proof of contradiction, assume that a tuple $t = (a_1, …, a_n)$ with $a_j : A_j$ and $t \in \mathrm{ownval}(T)$ exists. If it did, $T$ would not be abstract. Since $t$ is an own value of $T$, it follows from Property 1 that $a_i$ must be an own value of $A_i$. This contradicts the assumption that $A_i$ is abstract as abstract types have no own values. Hence, $t$ cannot exist and thus $\mathrm{ownval}(T) = \empty$. Ultimately, $T$ is abstract.

**Proof:** *If the product type is abstract then at least one component type is abstract.*

Let $T = (A_1, ..., A_n)$ be an abstract product type. Hence, we have $\mathrm{ownval}(T) = \empty$. We know from Theorem 1 that $\mathrm{ownval}(T) = \mathrm{ownval}(A_1) \times ... \times \mathrm{ownval}(A_n)$. If all $A_i$ had at least one own value, $T$ would also have at least one own value. To achieve the empty set, at least one factor in the Cartesian product must be empty itself, hence at least one component type must have an empty own value set and thus be abstract.

###### Examples

```
val t1: (String, String, Int) = ("Hello", "World", "v2")
val t2: ((Int, Int), Real) = ((1, 2), 5.44)
```



##### Function Types

**Function types** describe corresponding function values. That is, a function that maps an input value $a : A$ to an output value $b : B$ has the type `A => B`. This type constructor is right-associative.

A **parameter list** is represented as a tuple. So for example, if we have a function with three parameters `(a: A, b: B, c: C)` and a returned type `R`, its function type would be `(A, B, C) => R`. This is equally possible for the output type, so `R` might be a tuple such as `(D, E, (F, G))`.

###### Abstractness

A function type is **never abstract** as at least one function will always be definable for any input/output type combination, such as a function ignoring the input and returning an arbitrary constant.

###### Examples

```
val f: Int => Int = x => x * 2
> f(2) = 4
val hello: Person => String = p => "Hello, ${p.name}. How are you today?"
> hello(marco) = "Hello, Marco. How are you today?"
val currentWealth: Person => Real = p.wealth
> currentWealth(medianPerson) = 100000.0
```



##### Intersection Types

Assume an **intersection type** `T1 & ... & Tn` for some arbitrary number of types $n >= 2$. Any value $v$ that satisfies the typing $v : \texttt{Ti}$ for *all* $1 \leq i \leq n$ also inhabits the type $\texttt{T1 & ... & Tn}$. The type constructor is associative and commutative. We call any type $\texttt{Ti}$ a **component type**.

We also define a **construction operation** that constructs intersection types from sets of types. Let $\mathcal{T}_1, \mathcal{T}_2$ be sets of types. Then we define:
$$
\mathcal{T}_1 \otimes \mathcal{T}_2 = \{ t_1 \texttt{ & } t_2 \mid t_1 \in \mathcal{T}_1, t_2 \in \mathcal{T}_2 \}
$$


###### Abstractness

An intersection type is considered abstract iff **any of its component types are abstract**. Note that intersection types don't have an ownval set.

If we think of values inhabiting a type as sets, the intersection type would be akin to taking the set intersection between all value sets. If at least one of these sets is empty, the intersection is also empty, hence the intersection type would be abstract.

Note that there is a second case in which the intersection of values is incompatible, i.e. we have two non-empty sets that are disjunct. We would theoretically have to assign abstractness to such an intersection type, but this is not computationally feasible in all cases. And in any case, such a type would be useless for practical purposes, since no function could ever be called with such an input type and no value could ever be assigned to a variable having such a type, as no value could ever satisfy the constraints. The whole idea of abstractness is that we can specialize functions and work with the subtypes, but specializing such an intersection type would only reduce the value set, hence there still wouldn't be any values we could call a function with.

###### Should all intersection types be abstract?

Some people might object that *all* intersection types should be abstract since they strictly don't define their own values. Right? **Not at all.** We specifically need to have concrete intersection types. Take the following example:

```
class A { ... }
class B { ... }

function f(v: +A & +B)

// entity: Entity & +A & +B
f(entity)
```

Clearly, this code shouldn't compile. We can easily call `f` with the argument `entity`, which *will not* dispatch to any other function but the declared `f`. So how can this function be abstract? Only if we make the mistake and treat *all* intersection types as abstract. `+A & +B` must be treated as a concrete type, since `A` and `B` are both concrete. Anything else would be PL homicide.

###### Examples

```
// Moves the entity based on its relative amount of health.
function move(entity: +Position & +Health) = ...
```



##### Sum Types

A **sum type** `T1 | ... | Tn` for some $n \geq 2$ describes values $v$ that satisfy the typing $v : \texttt{Ti}$ for *any* $1 \leq i \leq n$. The type constructor is associative and commutative.

###### Abstractness

A sum type is **always abstract**. This might not be clear, so we have collected some reasons for this.

**Reasons:**

- **Pragmatic:** We need sum types to be abstract to be able to define abstract functions that can be specialized via multiple-dispatch. Take the following example:

  ```
  class Character { ... }
  type Class = 'Berserker | 'Druid | 'Occultist
  
  // Let's assume we create a character with a list of skills based
  // on a predetermined class.
  function create(c: Class): Character
  function create(c: 'Berserker): Character = { ... }
  function create(c: 'Druid): Character = { ... }
  function create(c: 'Occultist): Character = { ... }
  ```

- **Hierarchical:** As you can also see in the example above, sum types can model ad-hoc class hierarchies. The sum type itself acts very much like an empty abstract class.

- **No need to be concrete:** If a sum type is an input type for a function, like `create` above, it does not need to be concrete, as the specialization for the specific components will cover all possible values that inhabit the sum type.

###### Examples

```
type Option[A] = 'None | Some[A]
```



##### Class Types

**Class types** are *declared types* that describe user-defined data structures.

###### Abstractness

A class type is abstract if it has been **declared abstract**.



##### Component Types

###### Abstractness

A component type is abstract if its **underlying type is abstract**.



##### Label Types

**Label types** are *declared types* that describe values without defining any on their own. A concrete value can never have a label type as its only type, so when looking at types for concrete values, label types *always* occur in conjunction with an intersection type. We can declare parameters using only label types, but would then have to call other functions (which are, eventually, specialized) that can use the label type.

###### Abstractness

A label type is **never abstract**.

**TODO:** Why? Shouldn't it always be abstract?

- If they are always abstract, we can define an abstract function `f(v: Class & Label)` over a concrete class type Class that gets called with a dynamically specialized type. That is, we create an object of type Class, attach the label type Label, and call the abstract function. It won't be able to dispatch to subclasses, as the class doesn't need to have subclasses. So that's obviously not correct.
- **TODO:** Can we find a similar counterexample for *never abstract*?



##### Singleton Types

A **singleton type** describes exactly one shapeless value. A singleton type `T` has exactly one value associated with it called `t` (the lowerCamelCase variant of the UpperCamelCase name `T`).

A singleton type is **defined** as such:

```
singleton T
```

**TODO:** Should we add inheritance (pretty useful) and data/properties (questionable) to singleton types? Especially the former would allow multiple dispatch over singleton types.

**TODO:** `object` would be another possible name, too.

Singletons can also be defined **in-place** in other type expressions (for example in a return type):

```
A => ('B | 'C)
```

In such a case, the singleton type will become part of the namespace of the current type expression. In a method definition, this would be the namespace containing the method, for example.

###### Abstractness

Singleton types are **never abstract**.

###### Examples

Singleton types are especially useful in conjunction with sum types:

```
record User /* ... */
type LoginResult = User | 'UserNotFound | 'IncorrectPassword
val result: LoginResult = userNotFound
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

###### Abstractness

An envelope type is abstract if its **underlying type is abstract**.

###### Examples

```
envelope PhoneNumber(String)
envelope Players(List[Player])
envelope Height(Real)
```

Possible alternative syntax: `type Real as Height`

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



#### Type Properties

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