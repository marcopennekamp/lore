# Multi-Functions

**Multi-functions** are an exceedingly important feature of Lore. They allow specializing a function via any of the parameters, thereby allowing dynamic dispatch over more than one argument. This feature is also called **multiple dispatch**.

*Definition.* A **multi-function** is a set of all functions that share the same name. Such an individual function is also called an **instance** of its multi-function. 

Multi-functions can be **invoked** like ordinary functions. The actual function being invoked is chosen at runtime as the function that is most specific in the set of functions that can be invoked with the actual (dynamic) argument types. That is, we consider functions of which the argument types are subtypes of the parameter types and then choose the most specific function. We call this kind of invocation **multiple dispatch**.

Multi-functions are useful because they allow functions to be implemented with varying levels of specificity. They lend themselves well to a varying ensemble of features and concerns:

- **Single Dispatch** is supported natively, as multiple dispatch subsumed single dispatch.

- **Intersection Types** can be used to define a sufficiently specific type for a given operation without naming concrete value types. For example, we can define a function that accepts the intersection of two component types, thereby opening that function to any object that has these two components.

  Furthermore, varying degrees of increasingly specific intersection types can be used to **specialize** a function in different ways. This sufficiently specific type can be used to implement a function which would otherwise have been confined to a class hierarchy in single-dispatch languages.

- **Dynamic Specialization and Generalization** of values can be used to specialize or generalize the semantic type of a value *at runtime*. Since the actual function being called is chosen at runtime when calling a multi-function, we can write functions that implement an operation for a given temporarily specialized argument.

  *This feature is not planned for the first running language version.*

- **Extendability** is improved by the ability to define multi-functions *across* files and compilation units. This supports features such as C#'s extension methods or Scala's implicit classes in a concise and native way.

**In this chapter,** we will look at the syntax of function declarations and define functions and multi-functions. After laying out the basics, we will define the rules of multiple dispatch and examine constraints and edge cases. We will see how intersection types and extension methods can be used as suggested above.

*Note that the syntax is very much in flux.*



### Functions and Multi-Functions

```
func-def    --> func-head ['=' expr]?
func-head   --> 'function' id '(' func-params ')' [':' type]?
func-params --> func-param [',' func-param]+
             |  func-param?
func-param  --> id [':' type]?
```

#### Functions

*Definition.* A **function** $f$ is a mapping from an input type $\mathrm{in}(f)$ to an output type $\mathrm{out}(f)$. Each function has a **full name**, which we denote $\mathrm{name}(f)$. Note that the `id` of a function (as specified in the grammar above) is *not* necessarily equal to its full name, as the name could further be qualified with modules or packages (which we will introduce in a later revision of the spec), which are part of the full name of a function.

The **body** of a function is an *expression* $\mathrm{body}(f)$. The type of $\mathrm{body}(f)$ must be a *subtype* of $\mathrm{out}(f)$. A body may be empty, in which case we write $\mathrm{body}(f) = ()$ and call the function **abstract**. An abstract function may not be called at run-time.

The **input type** of a function $f$ is defined as follows: Let $[t_1, \dots, t_n]$ be the list of parameter types for each parameter $p_i$. Then we have $\mathrm{in}(f) = (t_1, \dots, t_n)$, that is, an n-tuple of the given parameter types.

We denote the **set of all possible functions** as $\mathbb{F}$.

---

*Example.* Consider the following definition for a function `add`:

```
function add(a: Int, b: Int): Int = a + b
```

We will call the defined function $f$. Then we have the following properties:

- $\mathrm{name}(f) = \mathrm{add}$
- $\mathrm{in}(f) = (\texttt{Int}, \texttt{Int})$
- $\mathrm{out}(f) = \texttt{Int}$
- $\mathrm{body}(f) =$ `a + b`

#### Multi-Functions

*Definition.* A **multi-function** is a pair $\mathcal{F} = (\mathrm{name}, F)$ where:

- $\mathrm{name}$ is the **name** of the multi-function (and all its associated functions).
- $F$ is the **set of instances** of the multi-function.

We define $F$ as follows: For any multi-function $\mathcal{F} = (n, F)$, we have $F = \{ f \in \mathbb{F} \mid \mathrm{name}(f) = n \}$. That is, a multi-function is a set of functions sharing the same name. By convention, we sometimes write $f \in \mathcal{F}$ for $f \in F$. We denote the set of multi-functions as $\mathbb{M}$.

---


*Example.* Consider the following function definitions:

```
function concat(x: ToString, y: ToString)           = ... // f1
function concat(x: List[a], y: List[a])             = ... // f2
function concat(x: LinkedList[a], y: LinkedList[a]) = ... // f3
```

Assuming no other function with the name `concat` exists, we have the multi-function $\mathcal{F}_\mathrm{concat} = (\mathrm{concat}, F)$ with $F = \{ f_1, f_2, f_3 \}$ being the set of `concat` functions defined above.



### Multiple Dispatch

To define **multiple dispatch** formally, we first need an operation that allows us to reduce the set of multi-function instances to only those functions that could be invoked with a given tuple of arguments.

#### Fit

*Definition.* The **fit** of a multi-function for a given argument type $t$ is the set of functions that could be invoked with a value of type $t$. We define the function $\mathrm{Fit} : \mathbb{T} \rightarrow \mathbb{M} \rightarrow \mathcal{P}(\mathbb{F})$—with $\mathbb{T}$ being the set of all possible types—as follows: 
$$
\mathrm{Fit}(t)(\mathcal{F}) = \{ f \in \mathcal{F} \mid \mathcal{in}(f) \geq t \}.
$$
That is, we look at all functions $f \in \mathcal{F}$ and choose only those whose **input types are a supertype** of the given argument type $t$. Hence, we choose functions which could be called with the given argument type. We cannot choose functions that have a more specific input type than the given argument type, because we need to invoke the function with valid arguments. 

We take functions with a **more general input type** into account, because such functions *can* be invoked with a subtype of the input type, i.e. with more specific arguments than needed. This is important for the case in which we cannot find a function that specifically meets the argument type $t$.

---

*Example.* Suppose we have the multi-function $\mathcal{F}_\mathrm{concat}$ defined earlier. We get the following results when applying $\mathrm{Fit}$:
$$
\mathrm{Fit}(\texttt{(String, String)})(\mathcal{F}_\mathrm{concat}) = \{ f_1 \}
$$
Only $f_1$ fits, because  $\texttt{List[a]}$ is not a supertype of $\texttt{String}$ and neither is $\texttt{LinkedList[a]}$. $\texttt{ToString}$ is a supertype of $\texttt{String}$ and hence $f_1$ fits.

Consider the next application of $\mathrm{Fit}$:
$$
\mathrm{Fit}(\texttt{(LinkedList[Int], List[Int])})(\mathcal{F}_\mathrm{concat}) = \{ f_1, f_2 \}
$$
Both $f_1$ and $f_2$ fit because the input types of both functions are supertypes of the argument types. $f_3$ does not fit since $\texttt{LinkedList[a]} \ngeq \texttt{List[a]}$ for the second argument type.

Finally, let's consider the following application:
$$
\mathrm{Fit}(\texttt{(LinkedList[Int], LinkedList[Int] & Sorted)})(\mathcal{F}_\mathrm{concat}) = \{ f_1, f_2, f_3 \}
$$
All three functions fit, because $\texttt{ToString} > \texttt{List[a]} > \texttt{LinkedList[a]}$ and $\texttt{LinkedList[a]} > \texttt{LinkedList[a] & Sorted}$, as the qualification with the intersection type makes the original type more specific.

#### Min

*Definition.* Let $\mathrm{Min} : \mathcal{P}(\mathbb{F}) \rightarrow \mathcal{P}(\mathbb{F})$ be the function that extracts the **most specific functions** from a given fit. We define:
$$
\mathrm{Min}(B) = \{ f \in B \mid \nexists f' \in B. \mathrm{in}(f') \leq \mathrm{in}(f) \}
$$
That is, Min extracts the most specific functions from a multi-function fit. Note that there may be multiple such functions if their input types are not comparable, or none at all if the fit is empty. We will explore both cases in the next example.

---

*Example.* Suppose we have the multi-function $\mathcal{F}_\mathrm{concat}$ defined earlier and a fit $B = \{ f_1, f_2 \}$. We apply $\mathrm{Min}$ as follows:
$$
\mathrm{Min}(\{ f_1, f_2 \}) = \{ f_2 \}
$$
This is because $\mathrm{in}(f_2) < \mathrm{in}(f_1)$, as lists are subtypes of ToString.

---

A Min-set with exactly one element is *the* result we need for multiple dispatch to be applicable. If the set was empty, we would not have found a suitable function to call. Perhaps even worse, if the set contains more than one element, we have an ambiguity and cannot decide which function to call. The following example shows that such an ambiguity exists.

---

*Example.* Assume we have the following two functions (note that at this stage, the syntax is just a placeholder):

```
function area(x: Circle) = // f1
  pi * x.radius * x.radius
function area(x: +BoundingBox) = { // f2
  const b = x.BoundingBox
  const width = b.maxX - b.minX
  const height = b.maxY - b.minY
  width * height
}
```

That is, we can calculate an area both for a circle and for an object that has a BoundingBox component. (This is not a particularly nice example, since a BoundingBox should not be used to calculate an area, but let's just say some wacky programmer decided to go with it.)

We can call the associated multi-function $\mathcal{F}_\mathrm{area}$. Now, what about a Circle that has a BoundingBox as a component? In other words, suppose we call $\mathcal{F}_\mathrm{area}$ with an argument of type $t = \texttt{Circle & +BoundingBox}$. We have the following properties:
$$
B = \mathrm{Fit}(t)(\mathcal{F}_\mathrm{area}) = \{ f_1, f_2 \} \\
\mathrm{Min}(B) = \{ f_1, f_2 \}
$$
For any types $\texttt{a}$ and $\texttt{b}$, it holds that $\texttt{a} \geq \texttt{a & b}$. Thus, we have $\texttt{Circle} \geq \texttt{Circle & +BoundingBox}$ and $\texttt{+BoundingBox} \geq \texttt{Circle & +BoundingBox}$. Hence the argument type fits both functions.

$\texttt{Circle}$ and $\texttt{+BoundingBox}$ are also *incomparable*, so neither is a subtype of the other. This means that both $f_1$ and $f_2$ have to be contained in the Min-set, as neither function can be ruled out given the argument type.

Since the most specific function is not unique, we must abort the compilation with an error (preferably) or even throw a runtime error.

In closing the example, we will look at the conditions needed to produce an empty fit. Let's assume we calculate the fit for $\mathcal{F}_\mathrm{area}$ called with an argument of type $\texttt{Rectangle}$. Provided that $\texttt{Rectangle}$ doesn't have a $\texttt{BoundingBox}$ component, the fit will be empty, because neither $\texttt{Circle}$ nor $\texttt{+BoundingBox}$ are a supertype of $\texttt{Rectangle}$.

#### Multi-Function Calls

Having defined the $\mathrm{Fit}$ and $\mathrm{Min}$ functions, we can finally turn our attention to defining *multi-function calls*.

*Definition.* Suppose we have a function call expression as follows:

```
N(e_1, ..., e_n)
```

Let $\mathcal{F}$ be a multi-function and $N$ its name. Let $t = (t_1, \dots, t_n)$ be the tuple type of the arguments $e_1 : t_1, \dots, e_n : t_n$. Let $B = \mathrm{Fit}(t)(\mathcal{F})$ be the fit of $\mathcal{F}$. Let $C = \mathrm{Min}(B)$ be the set of most specific functions in the fit $B$.

A **multi-function call** is an operation with compile-time constraints and run-time semantics:

- At **compile-time**, we need to check whether the function to call would be unique assuming $t$ was also the run-time argument type. Of course, one of the arguments $e_i$ might be a subtype of $t_i$, but we can only catch these problems at run-time. We can at least test that the type bound fits at all.

  We distinguish the following cases:

  - If $C = \empty$, there is no function that fits the argument type. We throw an `empty-fit` error.
  - If $|C| = 1$, that is, there is exactly one most specific function to call, the multi-function call passes the compile-time check.
  - If $|C| > 1$, that is, we have an ambiguity already at compile-time, we throw an `ambiguous-call` error.

- At **run-time**, the actual argument types might specialize the compile-time type bound $t$. Let $t' = (t'_1, \dots, t'_n)$ be the run-time input type. Let $B' = \mathrm{Fit}(t')(\mathcal{F})$ and $C' = \mathrm{Min}(B')$. The `empty-fit` error is always caught at compile-time (refer to the next section for a proof). Thus, we know that $C' \neq \empty$ at run-time, and so we only distinguish two cases:

  - If $|C| = 1$ with $f \in C$, we call the function $f$. Note that $f$ cannot be abstract because of the totality constraint on abstract function, defined in a following section.
  - If $|C| > 1$, we throw an `ambiguous-call` error.

Note that we have to distinguish between compile-time and run-time errors,
which we will talk more about in the next section. Also note that the compile-time constraints do not refer to the abstractness of the function
$f$, since the point of an abstract function is exactly that the compile-time
checks pass while we require specialization of the argument types at runtime.



### Empty-Fit and Ambiguous-Call Errors

In the definition of multi-function calls we had to **distinguish between compile-**
**time and run-time errors**. In particular, we defined the `empty-fit` error
as compile-time-only, while the `ambiguous-call` error may either occur at
compile-time or run-time. Of course, compile-time errors are always prefer-
able to run-time errors. However, with the expressiveness of the type system
of Lore, we can not get around the possibility of a run-time error for the
ambiguity case. We will see why in this section.

#### Proof: Empty-Fit can only occur at compile-time

First, let's look at the compile-time-only property of the `empty-fit` error. We will prove that the error can only occur at compile-time by looking at a call of a multi-function $\mathcal{F}$.

*Proof.* Let $t$ be the argument type deduced at compile-time. Let $B = \mathrm{Fit}(t)(\mathcal{F})$ and $C = \mathrm{Min}(B)$. Assume we call $\mathcal{F}$ at run-time with an argument type $t'$.  Let $B' = \mathrm{Fit}(t')(\mathcal{F})$ and $C' = \mathrm{Min}(B')$.

Assume that $C' = \empty$ but $|C| = 1$. That is, calling $\mathcal{F}$ at compile-time was valid, but we can't find any fitting function to call at run-time with the argument type $t'$.  Since $C \neq \empty$, we know that $B \neq \empty$, so there exists an $f \in B$ such that $\mathrm{in}(f) \geq t$ (by definition of $\mathrm{Fit}$). Now, we observe that $t'$ specializes the argument type $t$, so that $t  \geq t'$ holds. This is the only way in which the run-time type of the argument can change. In particular, $t$ can not be generalized, because it is the upper-bound for any actual argument types. Trivially, we have $\mathrm{in}(f) \geq t \geq t'$, hence $f \in B'$ (by definition of $\mathrm{Fit}$). Finally, we can derive that $f \in C'$ *or* that there must exist another function $g \in C'$ with $\mathrm{in}(g) < \mathrm{in}(f)$. In both cases, there is some function in $C'$, which contradicts $C' = \empty$. This proves that the `empty-fit` error can only occur at compile-time.

#### Proof: Ambiguous-Call can occur at run-time

Moving on to the `ambiguous-call` error, we can show with a simple example that such an error might only be caught at run-time. Let's look at the ambiguity example shown above again. We have two functions $f_1 : \mathtt{Circle} \rightarrow \mathtt{Real}$ and $f_2 : \mathtt{+BoundingBox} \rightarrow \mathtt{Real}$. At compile-time, in an expression `area(c)`, $f_1$ is the unique most specific function to call given a variable `c : Circle`. Since $\texttt{Circle & +BoundingBox}$ is a subtype of $\texttt{+BoundingBox}$, such an argument may be passed at run-time. The problem is that the additional intersection type allows both `area`-functions to be called, as could be seen when we computed the Min-set in the example. This leads to an ambiguity error at *run-time*.

More generally, we can observe that intersection types are precisely the feature that make multiple-dispatch ambiguous.



### Constraints on Return Types

So far we have only considered *parameter types* of multi-function instances. We also have to account for return types of functions.

First of all, we cannot incorporate the return type into the multiple-dispatch process. Doing so would jeopardize our type inference mechanism, which relies on stable function return types. To ensure type safety, we impose a constraint on return types.

*Definition.* Let $\mathcal{F}$ be a multi-function. The following **constraint on return types** must be satisfied for all $f, f' \in \mathcal{F}$:
$$
\mathrm{in}(f) \geq \mathrm{in}(f') \implies \mathrm{out}(f) \geq \mathrm{out}(f')
$$
That is, assume $f'$ specializes $f$. If we have a value $v$ with a compile-time type of $\mathrm{in}(f)$ in a function call `f(v)` , we can assume a return type of $\mathrm{out}(f)$. Since $f'$ is called instead of $f$ if $v$ is run-time specialized to $\mathrm{in}(f')$, we must ensure that the return type of $f'$ satisfies the type bound $\mathrm{out}(f)$.

If a multi-function fails to satisfy this constraint, an `invalid-return-type` error will be thrown at compile-time, mentioning all offending function definitions.



### Abstract Function Constraints

We want **abstract functions to guarantee that multiple dispatch always finds at least one concrete function** to call at run-time. To achieve this, we must ensure the following two properties:

1. The input type of an abstract function must be abstract itself, which is formalized in the **input abstractness constraint**.
2. Abstract functions must be covered by functions accepting all possible concrete subtypes of the input type, which is formalized in the **totality constraint**.

#### Input Abstractness Constraint

*Definition.* Let $\mathcal{F}$ be a multi-function. The following **input abstractness constraint** must be satisfied for all abstract function $f \in \mathcal{F}$:
$$
\mathrm{abstract}(\mathrm{in}(f))
$$
To appreciate the need for this constraint, consider the following: Assume we define an abstract function $f \in \mathcal{F}$ with an input type $\texttt{A}$, which is a concrete class type. The type $\texttt{A}$ has two direct subtypes $\texttt{A1}$ and $\texttt{A2}$. Even if we satisfied the totality constraint for $f$ by defining two implementations $f_1$ and $f_2$, we could call $\mathcal{F}$ with an argument $a : \texttt{A}$. This would attempt to call an abstract function at run-time, illustrating the need for the input abstractness constraint.

If an abstract function $f$ does not satisfy this constraint, an `input-type-not-abstract` error is raised.

#### Totality Constraint

*Definition.* Let $\mathcal{F}$ be a multi-function. The following **totality constraint** must be satisfied for all abstract functions $f \in \mathcal{F}$:
$$
\forall s < \mathrm{in}(f). \neg\mathrm{abstract}(s) \implies [\exists f' \in \mathcal{F}. \mathrm{in}(f') < \mathrm{in}(f) \and f' \in \mathrm{Fit}(s)(\mathcal{F})]
$$
That is, all concrete subtypes $s$ of $f$'s input type must be covered by at least one function $f'$ whose input type is a strict subtype of $\mathrm{in}(f)$. Together with the input abstractness constraint, this ensures that all input values with which the function can ever be called are dispatched to a *concrete* function.

If a multi-function $\mathcal{F}$ does not satisfy the totality constraint, a `missing-implementation` error is raised, which includes a list of input types that need to be covered.

---

Contrary to intuition, for the definition of the totality constraint, **it does not matter whether $f'$ is abstract or not**. If $f'$ is not abstract, the potential argument type $s$ is trivially covered by $f'$. If $f'$ *is* abstract, the totality constraint also needs to hold for $f'$. 

There is a valid concern: Can't we just define abstract functions down to the leaves of the dispatch tree and never provide an implementation? Yes! But the *input abstractness constraint* ensures that functions over concrete input types cannot be abstract themselves. This **guarantees an implementation *if* there are concrete subtypes** of a given abstract type. If there are no concrete subtypes, of course we want to allow defining functions without ever supplying an implementation; such an implementation can be, for example, provided by a library user after subtyping.

*Example.* To demonstrate what we **discussed above**, take the following constellation of functions:

```
abstract class A { }
class AI extends A { }
abstract class B { }
class BI extends B { }

function f(a: A, b: B)   // f1
function f(a: AI, b: B)  // f2
function f(a: A, b: BI)  // f3
```

This code would not compile. While `f1` is fully covered, neither `f2` nor `f3` are covered themselves and thus don't satisfy the totality constraint. Let's add another function `f4`:

```
function f(a: AI, b: BI) // f4
```

Now, the totality constraint is satisfied for all functions. But then the code compiles, which it shouldn't, right? *Wrong.* The *input abstractness constraint* makes `f4` invalid. The idea that functions need to be implemented for concrete values might not be encoded in the totality constraint, but it is still checked within the system.

#### Example

Suppose we have the following types:

```
class A { ... }
class B { ... }
type T = A | B
```

That is, $\texttt{T}$ is a sum type, while $\texttt{A}$ and $\texttt{B}$ are concrete class types. We also have the following two functions:

```
function f(a: A): A = { ... }
function f(b: B): B = { ... }
```

Suppose we have a value $v$ of type $\texttt{T}$. If we try to call `f(v)`, we will get an empty-fit error at compile time, because both functions are too specific for the more general type $\texttt{T}$. At run-time, of course, *we* are certain that one of the functions must be called, because $v$ must either have type $\texttt{A}$ or $\texttt{B}$. But of course the compiler doesn't know.

This demonstrates the usefulness of abstract functions. We define the following additional function to fix our issue:

```
function f(t: T): T
```

This function is abstract because it has no definition. It satisfies the totality constraint because the concrete subtypes $\texttt{A}$ and $\texttt{B}$ each have an associated concrete function. The input abstractness constraint is also satisfied as sum types are abstract by definition. (**TODO:** Is this true?) We can now call the function $f$ with a value $v$ without getting a compilation error.

Note that the *constraint on return types* as defined earlier is also satisfied:

1. For `f(a: A)`: $\texttt{T} \geq \texttt{A}$
2. For `f(b: B)`: $\texttt{T} \geq \texttt{B}$

This concludes our usage example about abstract functions.

#### Totality Constraint Verification

It is not obvious **how to check the totality constraint**. In this section, we will develop an algorithm that can verify the constraint for an arbitrary abstract function.

Following the definition of the constraint, the crux of the algorithm is to identify the set of subtypes that need to be checked. The check itself is trivial since it only involves finding a specialized function that fits the subtype.

We will see that this is entirely possible. First, we define a lemma that will allow us to ignore all added intersection types.

---

*Definition.* The **abstract-resolved direct subtypes** $\mathrm{ards}: \mathbb{T} \rightarrow \mathcal{P}(\mathbb{T})$ of any type $t$ are recursively defined as follows:
$$
\neg\mathrm{abstract}(t) \implies \mathrm{ards}(t) = \{ t \} \tag{concrete}
$$

$$
\mathrm{is\_declared}(t) \implies \mathrm{ards}(t) = \mathrm{sub}_{D_1}(t)
$$

$$
t = (t_1, \dots, t_n) \implies \mathrm{ards}(t) = \mathrm{ards}(t_1) \times \dots \times \mathrm{ards}(t_n)
$$

$$
t = (t_1 \texttt{ & } \dots \texttt{ & } t_n) \implies \mathrm{ards}(t) = \mathrm{ards}(t_1) \otimes \dots \otimes \mathrm{ards}(t_n)
$$

$$
t = (t_1 \mid \dots \mid t_n) \implies \mathrm{ards}(t) = \bigcup^{n}_{i = 1} [\mathrm{ards}(t_i)]
$$

Some **explanations** are in order:

- **concrete:** If $t$ is concrete, we do not return subtypes but only $t$ itself. Say we have an abstract function `f(a: A, b: B)` with `A` being abstract and `B` a concrete type. We don't want to force `B` to be specialized, as only `A` is abstract and therefore a function like `f(a: AI, b: B)` would be legal (assuming `AI` is a subtype of `A`). This rule includes function types, since they can never be abstract. In fact, we *have* to keep `B` as general as possible to ensure that values of `B` are also covered by an implementation and not just subtype values.
- Note that we are ignoring the `Any` type, since declaring an abstract function over it is pure madness. (**TODO:** Maybe we should just disallow using any with abstract functions altogether.)

---

*Theorem 2.1.* Let $\mathcal{F}$ be a multi-function and $f \in \mathcal{F}$ an abstract function. To check the totality constraint, it suffices to check the constraint for all *abstract-resolved direct subtypes* instead of all possible subtypes. Thus, we can **restrict the set of checked subtypes to a computable amount**. Formally:
$$
\forall s \in \mathrm{ards}(\mathrm{in}(f)). P(s) \implies \forall s < \mathrm{in}(f). P(s)
$$
with $P(s) \iff \neg\mathrm{abstract}(s) \implies [\exists f' \in \mathcal{F}. \mathrm{in}(f') < \mathrm{in}(f) \and f' \in \mathrm{Fit}(s)(\mathcal{F})]$.

*Proof.* We will take this proof in a few steps. From the premise, we can of course assume that all $s \in \mathrm{ards}(in(f))$ satisfy $P(s)$.

**(1)** First of all, we show the following property for all types $s$:
$$
s \in \mathrm{ards}(\mathrm{in}(f)) \implies (\forall s' < s. P(s'))
$$
Expand the definitions of $P$ and $\mathrm{Fit}$:
$$
\forall s' < s. P(s') \\
\iff \forall s' < s. (\neg\mathrm{abstract}(s') \implies [\exists f' \in \mathcal{F}. \mathrm{in}(f') < \mathrm{in}(f) \and f' \in \{ g \in \mathcal{F} \mid s' \leq \mathrm{in}(g) \}]) \\
\iff \forall s' < s. (\neg\mathrm{abstract}(s') \implies [\exists f' \in \mathcal{F}. \mathrm{in}(f') < \mathrm{in}(f) \and s' \leq \mathrm{in}(f')])
$$
Take the $f'$ for which $P(s)$ is true (there must be at least one according to the premise). We have $\mathrm{in}(f') < \mathrm{in}(f)$ and $s \leq \mathrm{in}(f')$. This same $f'$ fits $s'$, since $s' < s \leq \mathrm{in}(f')$, so we prove that $P(s')$ is true for all $s' < s$.

This already **shows the theorem *for a significant number of subtypes***. Our goal is now to identify the subtypes we still have to cover for the proof to be total. Those are:
$$
S = \{ s \mid s < \mathrm{in}(f) \and \neg\mathrm{abstract}(s) \} - \mathrm{ards}(\mathrm{in}(f)) - \{ s' \mid s' < s \and s \in \mathrm{ards}(\mathrm{in}(f)) \} \\
\iff S = \{ s \mid s < \mathrm{in}(f) \and \neg\mathrm{abstract}(s) \} - \{ s' \mid s' \leq s \and s \in \mathrm{ards}(\mathrm{in}(f)) \}
$$
**(2)** We want to show:
$$
S = \empty
$$
Therefore, we have to show:
$$
\forall s. (s < \mathrm{in}(f) \and \neg\mathrm{abstract}(s) \implies s \leq s' \and s' \in \mathrm{ards}(\mathrm{in}(f)))
$$
If that is true, the subtrahend subsumes the minuend and thus $S$ would be empty.

Let $s$ be any *concrete* subtype of $\mathrm{in}(f)$. We show that an $s' \in \mathrm{ards}(\mathrm{in}(f))$ exists for which $s \leq s'$ holds by *playing a game*.

The rules of this game are simple:

1. The **antagonist** constructs a *concrete* subtype $s$ of $\mathrm{in}(f)$ which is not in $\mathrm{ards}(\mathrm{in}(f)))$.
2. The **hero** proves that $s$ is actually a subtype of an $s' \in \mathrm{ards}(\mathrm{in}(f))$.

The statement is shown when the antagonist has exhausted all of her options. The base type from which the antagonist can construct a type shall be $t = \mathrm{in}(f)$, which is necessarily a tuple type. The antagonist can change the type in any way as long as the end product is a strict subtype of $t$.

We consider the following **turns:**

1. The antagonist recognizes that she **cannot insert new product types**, that is, transform a part of $t$ that is not already a product type into a product type, since then $s$ would not be a subtype of $t$. (Product types can only be subtypes of product types and Any.) The hero is delighted that the antagonist doesn't throw futile cases at her.
2. The antagonist tries to change an **abstract declared type** $d$ within $t$. To replace the declared type, she necessarily has to choose a strict subtype $d' < d$. But then the hero points out that $\mathrm{ards}(t)$ is already doing exactly that, so the antagonist's $s$ would be a subtype of at least one $s'$.
3. The antagonist chooses to change a **concrete declared type** to some subtype. But then, as $t$ must be abstract, the antagonist didn't get rid of the abstractness of $s$ and thus isn't making a valid turn for the purposes of the game. The hero points out as much. And if the antagonist changes both a concrete and an abstract declared type, case (2) holds.
4. The antagonist tries to narrow an existing part $d$ within $t$ with an **intersection type**. We define $d' = d \texttt{ & } u$ for some arbitrary type $u$. Considering intersection type subtyping rules, we have $d' < d$. The antagonist also has to replace all abstract types so that $s$ is concrete. Thus, we have a subtype $s < t$ that is *equal* to a type $s' \in \mathrm{ards}(\mathrm{in}(f))$ except that there is a part $d'$ instead of $d$. By the rules of subtyping, the hero points out that $d' < d \implies s < s' < t$ and thus the move isn't valid.
5. The antagonist, otherwise out of options, attempts to introduce a **sum type**. Taking any part $d$ within $t$, we define $d' = d \mid u$ for some arbitrary type $u$. But then, by the rules of subtyping, $d'$ is not a subtype of $d$; quite the contrary, as $d < d'$. Thus, if the antagonist introduces a sum type, $s < t$ does not hold and the antagonist cannot make this move.

Ultimately, the hero claims victory, showing that $S$ is empty. This means that there are **no strict subtypes of $\mathrm{in}(f)$ that are not a subtype of one of the types in $\mathrm{ards}(\mathrm{in}(f))$**. Thus, if we prove $P(s)$ for all types in $\mathrm{ards}(\mathrm{in}(f))$, we prove $P(s)$ *for all strict subtypes of $\mathrm{in}(f)$*.

**Theorem 2.1** has been proven.

---



