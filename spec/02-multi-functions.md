# Multi-Functions

**Multi-functions** are an exceedingly important feature of Lore. They allow specializing a function via any of the parameters, thereby allowing dynamic dispatch over more than one argument. This feature is also called **multiple dispatch**.

*Definition.* A **multi-function** is a set of all functions that share the same name. Such an individual function is also called an **instance** of its multi-function. 

Multi-functions can be **invoked** like ordinary functions. The actual function being invoked is chosen at runtime as the function that is most specific in the set of functions that can be invoked with the actual (dynamic) argument types. That is, we consider functions of which the argument types are subtypes of the parameter types and then choose the most specific function. We call this kind of invocation **multiple dispatch**.

Multi-functions are useful because they allow functions to be implemented with varying levels of specificity. They lend themselves well to a varying ensemble of features and concerns:

- **Single Dispatch** is supported natively, as multiple dispatch subsumed single dispatch.

- **Intersection Types** can be used to define a sufficiently specific type for a given operation without naming concrete value types. For example, we can define a function that accepts the intersection of two component types, thereby opening that function to any object that has these two components.

  Furthermore, varying degrees of increasingly specific intersection types can be used to **specialize** a function in different ways. This sufficiently specific type can be used to implement a function which would otherwise have been confined to a class hierarchy in single-dispatch languages.

- **Dynamic Specialization and Generalization** of values can be used to specialize or generalize the semantic type of a value *at runtime*. Since the actual function being called is chosen at runtime when calling a multi-function, we can write functions that implement an operation for a given temporarily specialized argument.

- **Extendability** is improved by the ability to define multi-functions *across* files and compilation units. This supports features such as C#'s extension methods or Scala's implicit classes in a concise and native way.

**In this chapter,** we will look at the syntax of function declarations and define functions and multi-functions. After laying out the basics, we will define the rules of multiple dispatch and examine constraints and edge cases. We will see how intersection types, dynamic specialization and generalization, and extension methods can be used as suggested above.



### Functions and Multi-Functions

```
func-def    --> func-head ['=' expr]?
func-head   --> 'function' id '(' func-params ')' [':' type]?
func-params --> func-param [',' func-param]+
             |  func-param?
func-param  --> id [':' type]?
```

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

---

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



