# Multi-Functions

**Multi-functions** are *the* core feature of Lore. From inheritance to behavioral abstraction to extendability and specialization of subtypes, everything hinges on the concept of multiple dispatch.

A **multi-function** is a set of functions bearing the same name, embedded in a specificity hierarchy. When a multi-function is called, the actual function to be executed is decided at run-time based on the argument types. In contrast to single dispatch, where such run-time function calls are decided over one argument (often called `this` in object-oriented languages), **multiple dispatch** generalizes this notion to any number of arguments.



### Functions and Multi-Functions

A **function** in Lore has a full name, a list of parameters, an input type, a return type (also called output type), and potentially an expression body. The full name includes the module name. Function names are normal identifiers, as described [here](identifiers.md). The input type is defined as the tuple type of all parameter types in the order of their declaration. The type of the result of the expression body, as well as the types of all values returned using `return`, must be subtypes of the function's return type. If the body expression is omitted, a function is considered **abstract** and may not be invoked at run-time. A function that is not abstract is also called *concrete*. 

An **action** is a function whose return type is `Unit`. Unless abstract, an action must have a block as its body, rather than any kind of expression. Conceptually, an action achieves results via side effects rather than a returned value.

A **multi-function** is a set of functions with the same full name. Each multi-function has its own specificity/dispatch hierarchy, which is used to choose the correct function to invoke during multiple dispatch.

###### Syntax Example

```
func foo(number: Int): Real = number * 1.5
func foo(string: String): String = '$string ???'

act bar(value: Real) do
  println(value)
end
```

This Lore code contains the functions `foo(Int): Real`, `foo(String): String`, and `bar(Real): Unit`. It contains two multi-functions `foo` and `bar`, the latter of which only has one function to invoke, making multiple dispatch for `bar` trivial. The `foo` multi-function is defined for `String` and `Int` single arguments. The return types of the three functions are `Real`, `String`, and `Unit`. Their expression bodies are `number * 1.5`, `'$string ???'`, and `do println(value) end`.

##### Constraint on Return Types

Take two functions *f1* and *f2* from a multi-function with *f2* specializing *f1*. Then *f2*'s **return type must be a subtype** of *f1*'s return type. This is easy to see. If at compile-time multiple dispatch would choose *f1*, the compiler uses the return type of *f1* to infer and check types. Hence, *f2* as a specialization must adhere to the substitution principle and only return values that adhere to *f1*'s return type.

##### Type Parameters

A function can additionally declare **type parameters** with an inline `where` clause or `@where` annotation:

```
func identity(x: A): A where A = x

@where A, B, C >: A <: B
func foo(a: A, b: B, c: C): Boolean = true
```

This is not a property of multi-functions but **individual functions**. As subtyping and fit have been extended for type variables, Lore can decide the specificity of two functions even if one of them is parametric and also do so at run-time during multiple dispatch. More on that later.

Note that type variables can only be used in bounds if they are declared **preceding** the bound. So in the example above, `A` and `B` must be declared before `C`.

The usual syntax for function type parameters in many programming languages is: `foo<A, B, C>(...)`. This is *not* possible in Lore, because functions are dispatched to at run time and thus the compiler cannot anticipate which function is being called. Hence, **direct assignments** to function type parameters are impossible. Even a `.fixed` call simply performs multiple dispatch at compile time, without directly assigning type parameters.

The **annotation syntax** `@where` has the advantage that it's clearly separated from the function head, which improves readability. The `@` in front of `where` also helps to put initial visual focus on the function head. In contrast, the **inline syntax** `where` is more concise and provides better visual clarity when only one or two type parameters without bounds are desired. The inline syntax and annotation syntax cannot be used at the same time.

Because type parameters are assigned from argument types, all type parameters declared with a function must be **contained in at least one parameter type**. Function declarations such as this are illegal:

```
@where A, B
func apply_to(a: A): (A => B) => B = f => f(a)
```

##### Unnamed Parameters

A **parameter name** may be omitted if it's not used within the function's body. Only the parameter type has to be specified. Unnamed parameters are especially useful for abstract functions, where parameter names are often redundant or difficult to come up with.

###### Example

```
func transcribe(DnaNucleotide): RnaNucleotide
func transcribe(#a): #u = #u
func transcribe(#t): #a = #a
func transcribe(#g): #c = #c
func transcribe(#c): #g = #g
```

The first function is abstract. A parameter name for `DnaNucleotide` would only add verbosity, e.g. `dna_nucleotide`, `nucleotide`, or just `n`. None of these candidates would add any additional information.

The last four functions only need to match on the type of the argument, instead of the argument itself (this is trivial for symbol types). Hence, parameter names can also be safely omitted here.

##### Compile-time Usage Disambiguation

Lore makes it possible to import and declare two or more multi-functions with the same simple name. For example, it's possible to import both `lore.list.get!` and `lore.option.get!`, and let the compiler disambiguate specific usages at *compile time*. See *Name Resolution for Multi-Functions* in [modules](modules.md) for more information.



### Multiple Dispatch

To define **multiple dispatch**, we need to deal with the specificity of individual functions and their relation to argument types.

##### Fit and Specificity

In the context of multiple dispatch, **fit** is *almost* defined as subtyping. The technical differences between subtyping and fit between two types (whether one type "fits into" another type) are limited to type variables. If the two types are monomorphic, fit is equal to subtyping. We won't go into these technicalities here. If a type *t1* fits into a type *t2* but not vice versa, we say that *t1* is **more specific** than *t2*.

###### Example

```
@where A, B <: A
func append(list: [A], element: B): [A]
```

This is not the signature of the actual appends operator in Lore, but this function will help us demonstrate polymorphic fit. The function `append` has the input type `([A], B)` for the type variables as defined. If we want to call the function, we will have to ensure that the argument types fit into the input type. Say we have an argument type `([Animal], Fish)`. Lore will know to assign `Animal` to `A` and `Fish` to `B`. Since `Fish <: Animal`, the variable assignment is legal and the function can be called with the given arguments. This is subtly different to subtyping, because `Animal` is not a subtype of `A`: it would have to be a subtype of *all possible instances* of `A`. `Animal` can merely be assigned to `A`, making the given arguments fit.

##### Multi-Function Fit and Min

The **fit of a multi-function** for a given argument type *t* is the set of functions that could be invoked with an argument of type *t*. This fit is not to be confused with the fit operator described in the previous section, but closely related.

Once the fit is calculated, which will almost always contain multiple candidates, we need to know which concrete function to call. The most specific function must be called with the arguments. This is the task of the **min operation**: it takes a set of functions (the fit) and returns only those functions whose input types are most specific. Note that there may be multiple if their input types are not comparable, or none at all if the fit is empty.

###### Example

```
trait Animal
trait Mammal extends Animal
trait Fish extends Animal

object Human extends Mammal
object Guppy extends Fish
object ManBearGuppy extends Mammal, Fish

func kind(Animal): String
func kind(Mammal): String = 'mammal'
func kind(Human): String = 'idiotic'
func kind(Fish): String = 'fish'
```

What's the **fit and min set** of a call `kind(human)` given `human: Human`? The fit contains `kind(Animal)`, `kind(Mammal)`, and `kind(Human)`, while the min set contains only `kind(Human)`, because `Human` is more specific than both `Animal` and `Mammal`. This is good news!

What's the **fit and min set** of a call `kind(hybrid)` given `hybrid: ManBearGuppy`? The fit contains `kind(Animal)` (since `ManBearGuppy <: Mammal <: Animal`), `kind(Mammal)`, and `kind(Fish)`. However, the min set contains both `kind(Mammal)` and `kind(Fish)`, since `ManBearGuppy` is both a mammal and a fish and neither is more specific than the other. This is bad! We will see in the next sections what happens in such cases.

##### Multi-Function Calls

A min set with **exactly one function** is *the* result we need for multiple dispatch to be well defined. If the set was empty, we would not have found a suitable function to call. Perhaps even worse, if the set contains more than one element, we have an ambiguity and cannot decide which function to call.

So during a **multi-function call**, the dispatcher first computes the fit of the call and then the min set. (This is not necessarily what an optimized dispatcher actually does, but illustrates the basic workings of a simple dispatcher.) If there is exactly one function in the set, the function is executed. Otherwise, one of two errors occurs:

- **Empty fit:** There are no functions to call. This kind of error is almost always caught at compile-time. Only when type variables with lower bounds are involved can the compile-time detection fail.
- **Ambiguous call:** There are too many functions to call! This kind of error is tricky to catch at run-time, because Lore enables arbitrary specialization of types with intersection types. It is always possible that an unforeseen combination of traits (all supertypes of a single given object), for example, leads to an ambiguous call, while no ambiguity was detected at run-time.

In general, the compiler will do its best to catch these errors at compile time, but we cannot make any generalized guarantees.

###### Example

In our `ManBearGuppy` example above, we got an ambiguous-call error when calling `kind` with a `ManBearGuppy` argument. We can resolve this problem with the following additional function:

```
func kind(hybrid: ManBearGuppy): String = 'unidentified hybrid'
```

The cool thing about Lore is that we can place that function (and struct definition) in any file. `Animal`, `Mammal`, `Fish`, and `kind` might be defined in a library, while we might want to add `ManBearGuppy` as an additional hybrid. Instead of having to change the library's source code, we can simply define the struct and function in our own code and the power of multiple dispatch glues everything together neatly.

##### Parametric Types

As seen above, Lore functions can declare type variables. **Parametric multiple dispatch** works by assigning parts of the actual argument types to type variables in the parameter types. If the assignment is successful, type variables are consistent (multiple occurrences of the same type variable must have been assigned an equal type), and the subtyping check holds, the argument type fits the given function and the function can be called with the given argument.

Take the following function **example:**

```
@where A
func append(list: [A], element: A): [A]
```

When `append` is called via multiple dispatch, the unbounded type variable `A` is assigned some type based on the actual argument types. The arguments must agree *at run time* such that `A` is consistently assigned the same type. If, given traits `X` and `Y extends X`, we called the function with an input type `([X], Y)`, this (naively defined) append function **would not be dispatched to**, because `X` and `Y` are not the same type, even though they agree in principle (and the function would be callable like that outside a multiple-dispatch universe). Rather, we have to define the following function:

```
@where A, B <: A
func append(list: [A], element: B): [A]
```

This function does not assume the list and element to agree. Rather, it expects the element to be some subtype of the list's element type, which still allows us to append the element to the list as lists are covariant and immutable. In this example you have also seen the usage of an upper bound. Type variables that are declared before other type variables (such as `A` being declared before `B`) can be used in subsequent bounds.

Since type variables are assigned by multiple dispatch, assigning type variables manually during a function call is impossible. A call such as this, with imaginary syntax, is simply impossible:

```
append[Real]([1, 2, 3], 4.5)  // Impossible code!
```

Of course, this is quite idiomatic in languages like Scala. We should be able to force the type variable if it isn't being inferred correctly, right? But with multiple dispatch, we are moving type variable assignment to the runtime, because it is essentially part of the dispatch decision.

On top of that, the execution engine makes **type variable assignments available at run-time**. This is, for example, used with the list appends operator to construct a proper type for the resulting list. It's also crucial to have access to type variable assignments to construct parametric structs. A struct, carrying its actual type information at run time, needs to be instantiated with the actual type carried in a type variable (say, `Int`) instead of the variable itself (`A`). This cannot be decided at compile time and thus needs to be handled by the execution engine.



### Abstract Functions

**Abstract functions** are essentially functions missing an implementation. At run time, if the compiler should screw up, trying to call an abstract function (without any suitable specializations available) will lead to an error. It is our goal to catch most violations tied to abstract functions at compile time. The core verification is handled by two constraints: input abstractness and totality.

##### Input Abstractness Constraint

The **input abstractness constraint** states that the input type of an abstract function must be abstract. The need for this constraint can be put quite simply: if a function that accepts a concrete input type has no implementation, there is at least one value for which that function is the most specific function to choose, even if there are many specializations. A type is concrete if values inhabit that exact type and none of the direct subtypes. So we would end up in a scenario where a function missing an implementation is *the* function to call.

##### Totality Constraint

The **totality constraint** states that if a function *f* is abstract, all concrete subtypes of *f*'s input type must be covered by at least one function *f2* that specializes *f*. This ensures that all values with which the function can ever be called are dispatched to a concrete function.

**Checking this constraint** is not trivial, especially in complex type hierarchies. At the current time, Lore cannot guarantee that the constraint catches all possible illegal scenarios, nor can it guarantee that there are no false negatives. We are putting practicality before theoretical correctness. Usage of the language in practice will demonstrate the range of issues that might crop up around this specific constraint.

##### Examples

###### Example 1

To demonstrate what we discussed above, consider the following functions:

```
trait A
object AI extends A

trait B
object BI extends B

func f(a: A, b: B)   // f1
func f(a: AI, b: B)  // f2
func f(a: A, b: BI)  // f3
```

This code does not compile. While `f1` is fully covered, neither `f2` nor `f3` are covered themselves and thus don't satisfy the totality constraint. Let's add another function `f4`:

```
func f(a: AI, b: BI) // f4
```

Now, the totality constraint is satisfied for all functions. But then the code compiles, which it shouldn't, right? *Wrong.* The *input abstractness constraint* makes `f4` invalid. The idea that functions need to be implemented for concrete values might not be encoded in the totality constraint, but it is still checked within the system.

###### Example 2

```
object A
object B

func f(a: A): A = ...
func f(b: B): B = ...
```

Suppose we have a value `v` of type `A | B`. If we try to call `f(v)`, we will get an empty-fit error at compile time, because both functions are too specific for the more general type `A | B`. At run-time, of course, *we* are certain that one of the functions must be called, because `v` must either have type `A` or `B`. But of course the compiler doesn't know.

This demonstrates the **usefulness of abstract functions**. We define the following additional function to fix our issue:

```
func f(v: A | B): A | B
```

This function is abstract because it has no definition. It satisfies the totality constraint because the concrete subtypes `A` and `B` each have an associated concrete function. The input abstractness constraint is also satisfied as sum types are abstract by definition. We can now call `f(v)` without getting a compilation error.

###### Example 3

```
trait Animal
func name(Animal): String

trait Fish extends Animal

object Bass extends Fish
func name(Bass): String = 'Bass'

object Trout extends Fish
func name(Trout): String = 'Trout'
```

Abstract functions do not need to be redeclared for subtypes. The example above demonstrates this by having the trait `Fish` omit a declaration of the multi-function `name`. The constraint's checking algorithm for `name(Animal)` will try to find a function `name(Fish)` and upon failure, assume that it is implicitly abstract. The totality constraint will then be checked given the input type `Fish`. 

Any errors will be reported for the most specific abstract function. If the `name(Bass)` function wasn't declared, the compiler would report a missing implementation for the input type `Bass` with the `name(Animal)` function, not the implicit `name(Fish)` function.



### Fixed Functions

Sometimes, we want to bypass run-time dispatch and ensure that we call a specific function regardless of the argument type at run time. A **fixed function expression** looks like this:

```
f.fixed[T1, T2, ...]
```

`T1, T2, ...` represent the desired input types. The compiler will choose the callee at compile-time using the standard dispatch mechanism and assemble it as a function value with the appropriate function type.

###### Example

The most obvious example is invoking a **"super"** function of some other, more specialized function. Here is the basic pattern:

```
// Assume types A, A1 <: A, A2 <: A, and some type R.

func f(a: A): R = do
  // ... some general implementation
end

func f(a: A1): R = do
  // ... some actions specific to A1
  // Then call the "super" function to handle the general case.
  f.fixed[A](a)
end

func f(a: A2): R = do
  // First call the "super" function to handle the general case.
  let result = f.fixed[A](a)
  // ... some actions specific to A2
  result
end
```

Having to specify the exact type isn't great for this use case. We might add an actual super keyword with syntactic sugar at some point. This requires the function to have only one dispatch hierarchy parent, however. 



### Practical Uses

Multi-functions and multiple dispatch have a multitude of **practical uses**. Truly, they are *the* core of Lore, and thus at the center of many language features and best practices.

*Note: This section will be expanded in a future version of the specification.*

##### Single Dispatch

Using multiple dispatch, we can of course perform **single dispatch**. This seems to be akin to building a 2D game with a 3D engine. Perhaps the point where this comparison breaks is that a single dispatch function hierarchy is probably the most common use case in Lore. However, even if you only dispatch on a single argument, Lore's form of multiple dispatch is more powerful than most single dispatch of object-oriented languages.

In most object-oriented languages, methods are intrinsically tied to the type hierarchy. Often, methods have to be defined within their owning classes. Single dispatch is performed by looking at the actual type of an object and choosing the concrete method that the object itself implements. In some languages like Scala, methods can be mixed in, but the resulting structure still boils down to data/behavior co-locality.

In Lore, you can define a type hierarchy with behavior such as presented in this first example:

```
trait Target

trait Vehicle
act move(Vehicle, Target)

trait Car extends Vehicle
act move(Car, Target)

trait Train extends Vehicle
act move(Train, Target)
```

This is a fairly standard example of single dispatch. Note that all actions so far are declared abstract. We can create a struct that extends the `Car` trait:

```
struct SmartCar extends Car
act move(car: SmartCar, target: Target) do
  // ...
end
```

Now, data (types) and behavior are completely orthogonal. You can define them separately from each other and only marry them when it's needed. Even if you only or mostly use Lore's multiple dispatch on one argument, you will find that this style of writing functions is far more flexible than the object-oriented class/method approach.

So let's assume that we have two kinds of targets: `GpsCoordinates` and `Directions`. Our smart car can deal with both (either taking GPS coordinates or directions through verbal input). Instead of pattern-matching the `target` argument (as one might approach it in a language like Scala, since we cannot go beyond single dispatch), we can simply specialize the `move` function:

```
act move(car: SmartCar, target: Target) do
  // ... fallback behavior
end

act move(car: SmartCar, target: GpsCoordinates) do
  // ... use GPS and self-driving capability to move to the coordinates.
end

act move(car: SmartCar, target: Directions) do
  // ... understand and execute the driver's verbal directions.
end
```

When `Vehicle` was declared, it might have never been anticipated that we'd need to dispatch on the `Target` argument, too. In Lore, we can do so at any time. Perhaps the `Directions` type was even introduced later (as a new feature for the self-driving software) and software engineers were lucky enough to be using a language with multiple dispatch: Lore allows you to build on top of existing concerns in a natural manner, and you end up being not *as* constrained by the expression problem.
