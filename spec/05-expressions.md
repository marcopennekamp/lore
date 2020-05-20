# Expressions

[Previous File](04-classes-entities-components.md) [Next File](06-compiler.md)

This document outlines all valid **expressions** found in Lore.

**TODO:** How do we implement type casts / conversions?

**TODO:** Consider introducing Swift-style `guard` statements with a twist: They operate within blocks. If the condition is false, continue the code, otherwise *return the value of the else part from the block*. I think this could be super useful in game development.



### Top-Level Expressions

**Top-level expressions** are a special type of expression which may only appear in the following places:

```
// They are legal at the top level of blocks.
{ const x = 0 }

// They are legal as the body of conditionals and repetitions.
if (y == 0) x = 0 else x = 5
repeat while (i < 10) i = i + 1
for (entity in entities) count = count + 1
```

Top-level expressions are currently **variable declarations**, **assignments**, and **yields**.



### Numbers

**TODO:** Can `Real` and `Int` be extended?

Lore supports **real** and **integer** numbers. Their types are `Real` and `Int`. `Int` is a subtype of `Real`. They will both be implemented using the standard Javascript number type, and so precise integers can only be guaranteed up to [MAX_SAFE_INTEGER](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Number/MAX_SAFE_INTEGER). We believe this is sufficient for all common use cases. If larger numbers than $2^{53}-1$ are desired, we recommend using a big integer implementation.

We are not introducing integers for performance reasons. Rather, we think that it is useful to **state intent with the type:** if a function expects a `Real` number, it can be any number, while a function expecting an `Int` is explicitly requiring the number to be an integer. This leads to self-documenting code and guarantees that are useful when comparing for equality and such.

Internally, integers are represented as reals, but with additional **guarantees and checks:**

- The compiler guarantees that integer **literals** have `Int` shape.
- The compiler guarantees that integer **operations** evaluate to `Int`.
- The compiler guarantees that functions **returning** `Int` have integer shape.
- **Reals** cannot be passed as integers and need to be manually cast down.
  - Since there are no precision issues, there is no need to require manual casting of integers to reals.
- Integer **function arguments** and **return values** are asserted, at runtime, to be an integer using the Javascript function `isInteger`. This assertion is meant to catch holes in these checks and guarantees.
- If we dynamically **dispatch** on the `Real` type, the actual type at runtime is determined using `isInteger`. If true, we dispatch with the value having `Int` as its type, and otherwise having `Real` as its type.

For now, we want to keep literal grammar to a minimum. Hence, we do not support scientific notation and only support base 10. Here are the **valid number formats:**

- **Integers:** `x` or `-x`, x being any number from 0 to MAX_SAFE_INTEGER.
- **Reals:** `x.y` or `-x.y`, with both x and y being numbers. We do not allow notations such as `.0` or `1.`.

##### Arithmetic Operators

The following **arithmetic operators** can be used on numbers. Note that the remainder operator will be implemented as a function.

```
a + b  // Addition
a - b  // Subtraction
a * b  // Multiplication
a / b  // Division
-a     // Negation
```

Arithmetic operations have the following **conversion rules:** 

- If both arguments are real or integer, the result is also of the same type.
- If one of the arguments is real, the result will also be real.

##### Equality and Order

Numbers are equal and ordered in accordance with the rules of sanity. Integers and reals can be mixed in comparisons without any issues.



### Booleans

**TODO:** Long-term, we could define booleans within Lore as singleton types.

**TODO:** We could make `Boolean` abstract, introduce two subtypes `True` and `False`, and allow dispatching on boolean "types". This is definitely ~~a whacky~~ an idea, and either very cool or very stupid. One problem I see is that when a boolean could be dispatched on as a flag, what we really want is a label type: `function contains(list: [a], value: a, isSorted: Boolean)` and `function contains(list: [a], value: a, isSorted: True)` etc. compared to `function contains(list: [a], value: a)` and `function contains(list: [a] & Sorted, value: a)`.

Lore supports **booleans**. Their type is `Boolean`. We will implement them using the standard Javascript boolean type. There are two boolean **values:** `true` and `false`. 

##### Logical Operators

The following **logical operators** can be used on booleans. All arguments have to be `Boolean` values.

```
a & b  // Conjunction
a | b  // Disjunction
~a     // Logical Not
```

We define *logical not* using a **tilde** so we can reserve the `?!` combo of characters for handling optional values.

**TODO:** We could consider, once we have introduced nullable/optional values (`Int?`), to turn the logical operators into operators that accept any argument types and return "truthy" values. Compare to Clojure, Elixir, or Javascript.

##### Equality and Order

True is equal to true, false is equal to false. Booleans are not ordered.



### Strings

Lore supports UTF-8 **strings**. Their type is `String`. We will implement strings using the standard Javascript string type. Javascript's string functions will *not* be available by default; instead, Lore will define its own functions.

Conceptually, a string is *not* a list of characters. **A string is just a string.** If you access a single character at a specific index either with iteration or the `character` function, you will get another string. We believe this is a more unified framework than adding a type just for characters. (Not to mention that, if we called a type `Character`, we'd clash with game developers who might want to have a type `Character`. We'll implement namespaces, of course, but even then, it'll be useful not to have this type name reserved.)

A string is always written within **single quotes**: `'text'`. We reserve the ability to use the double quotes symbol for string-related features later on or something else entirely.

Strings are **interpolated** by default. You will be able to use `${expr}` for complex expressions, but for now `$s`, the shorthand for single variables, is the only way to interpolate strings.

The following **escaped characters** are available: `\n`, `\r`, `\t`, `\'`, `\$`, `\\`, as well as Unicode escapes such as `\u0008`.

We will add **multi-line strings** in another version of Lore.

###### Example

```
const k = 10
const p: Person = ...
const announcement = '${p.name}, you have $k apples. Please claim your ${if (k < 10) 'free' else '1000\$'} apple at the reception.'
```

##### String Operators

One might expect the plus operator to support **string concatenation**. This is not the case in Lore. In most cases, *interpolation* will be the preferable option compared to operative concatenation. In all other cases, most likely when you're working algorithmically with strings, concatenation is provided as a function.

(**Note:** This will obviously change when we introduce user-defined operators.)

##### Equality and Order

Two strings are equal if they have the same length and characters. Strings are ordered alphabetically.



### Tuples

Lore supports **tuples**. As described by product types, tuples are fixed-size, heterogenous lists of values. Tuples are simply created by putting parentheses around comma-separated values: `(a, b, c)`. A tuple value's type is the product type of the respective element types.

###### Example

**TODO:** How can we define `get` such that it supports tuples of arbitrary length? Give every product type a supertype called `Product` and implement `get` via multiple dispatch?

```
const t = (a, b, c)
get(t, 0) // a
get(t, 2) // c
```

##### Unit

Lore supports a **unit** value, which is simply the empty tuple. It is written `()` and has the type `()`. The unit value is special in Lore, as it is the *default return type* of functions. If you do not declare a return type, Lore assumes you don't want to return anything, and so the empty tuple is returned. Additionally, unit is the return type and value of variable assignments.

##### Equality and Order

Two tuples are equal if they have the same size and their elements are equal.



### Lists

Lore supports **lists** as first-class constructs. A list is a homogenous, linear collection of an arbitrary number of elements. List types are denoted `[A]`. For now, lists are implemented using Javascript arrays, so they are vector-like structures. We will eventually differentiate between indexed lists and linked lists.

You can **construct** a list by putting comma-separated elements inside square brackets: `[a, b, c]`. The empty list is denoted simply `[]`.

Later, we can add **list comprehensions**.

##### Equality and Order

Two lists are equal if they have the same lengths and each of their elements, considered in order, are equal. Lists are not ordered by default.



### Maps

Lore supports **maps** as first-class constructs. A map is a homogenous, indexed collection of key/value pairs. Map types are denoted `A -> B`. For now, maps are implemented using initially empty Javascript objects.

You can **construct** a map with the following syntax: `%{ k1 -> v1, k2 -> v2, k3 -> v3 }`. The empty map is denoted `%{ }`.

**Note:** Native **Javascript objects** will likely be represented as maps with a string key and a dynamic value type. Of course, we have not yet introduced the dynamic type, and so this will not be possible just yet.

###### Example

We can define a map from strings to integers:

```
const points = %{ 'Ameela' -> 120, 'Bart' -> 14, 'Morrigan' -> 50 }
// points: String -> Int
```

##### Equality and Order

Two maps are equal if for each key/value pair in the first map, there is a key/value pair in the second map, and vice versa. Maps don't define an order by default.



### Objects

Lore supports **object instantiation**, i.e. the construction of an object value given a class constructor. The syntax is:

```
// Assuming another value b: B
const a = A(b)        // Default constructor
const a = A.fromB(b)  // Named constructor 'fromB'
```

##### Equality and Order

Object equality is defined as **referential equality** by default.



### Property Access

You can access the **property** of an object with the `.` notation. The type of the expression is the type of the property. The syntax is simply: `object.property`.

Although the dot notation will eventually be overloaded for multi-function invocation, **property access takes precedence**. In such a case, you can always invoke the multi-function without using the dot-notation.



### Comparison Operators

Lore supports the following **comparison operators:**

```
a == b   // Equality
a =/= b  // Inequality
a < b    // Less than
a <= b   // Less than or equal
a > b    // Greater than
a >= b   // Greater than or equal
```

To **define equality** for a given type, you can specialize the function `isEqual(a, b)`. Inequality is strictly defined as `~isEqual(a, b)`.

```
function isEqual(c1: Car, c2: Car): Boolean = ...
function isEqual(c1: SportsCar, c2: CheapCar): Boolean = false
```

To **define order** for a given type, specialize the function `isLessThan(a, b)`. The *greater than* operator is strictly defined as `~(a < b) & a =/= b`.



### Blocks

A **block** is a sequence of expressions, the last of which becomes the value of the block. Yes, *blocks are expressions*. You can write code like this:

```
const results = {
  const a = 5
  const b = 10.0
  const c = getReason()
  if (c == 'business') a * b else a / b
}
```

Blocks also give you the luxury of **lexical scoping**, so make sure you declare variables exactly where you need them. In the example above, neither a, b, nor c are visible outside the block, so you don't need to pollute the outer scope with some truly temporary variables.



### Multi-Function Calls

**TODO:** In a later version of Lore, we can support a feature such as Swift's trailing closures, maybe add some way to pass two or more closures.

**Multi-Function calls** are the heart of Lore. Their syntax is simple:

```
name(a1, a2, ...)
```

That's it! Types will be checked, values will be dispatched, and some function will be called. The **semantics** of multi-function calls are defined in [multi-functions](03-multi-functions.md).

We say *multi-function call*, because it only becomes a function call at run-time, when a function has been chosen according to the dispatch semantics. At compile-time, we are calling a whole multi-function with a bounded but unknown input type.

##### Fixed Invocation

Instead of dispatching at run-time, you can **fix a function to call at compile-time**, as defined in *Fixing the Callee at Compile-Time*. The syntax for this is as such:

```
f.fixed[T1, T2, ...](a1, a2, ...)
```

In the long term, `f.fixed[T1, T2, ...]` is supposed to return a function value. As we are not supporting these for now, the whole syntax is parsed and compiled as one big expression.



### Native Function Calls

Many functions in the Lore standard library, especially the most fundamental ones, defer their implementation via **native function calls**. Since the underlying runtime environment is a Javascript environment, when defining a function implementation, we can also call functions from Javascript.

The **syntax** of a native function call is as follows:

```
name.native(a1, a2, ...)
```

For now, the Lore compiler will trust the programmer that, when named, such a function is actually available, takes the given arguments, and returns a value that conforms to the required type bounds. In that sense, this is a **dynamic call**.

In the long term, we want to be able to use, for example, TypeScript declaration files to provide more **type safety** when interfacing with Javascript or TypeScript code. Also note that, as of now, special Lore objects are passed to a native function, which are Javascript values additionally wrapped in an object to provide type information at run-time. Only primitives such as numbers and strings are unwrapped. A native function will have to deal with these idiosyncrasies until we have come up with a native unwrapping solution. 



### Conditional Expressions

Lore will support a variety of **conditional expressions** (especially switch expressions). For now, we will have to make do with If:

```
if (cond) statement2 else statement2
```

Note that either `statement` (or even `cond`) may be a block. The else part is, of course, optional. The so-called **dangling else** is always parsed as belonging to the `if` closest to it.



### Repetition

**TODO:** Rename to Loops. Otherwise the general name is too close to repeat while.

**Repetitions** represent repeating control flow. For now, we support loops and iteration.

##### Loops

In Lore, a **loop** repeats some piece of code until a given boolean expression is false. This is expressed by the archetypal Repeat-While:

```
repeat while (cond) statement
repeat statement while (cond)
```

The first version checks the condition first, then executes the statement (if the condition is true). The second version executes the expression first, then checks the condition (and terminates the loop if the condition is false).

##### Iteration

An **iteration** iterates over some kind of collection. We define the archetypal For as such:

```
for (e1 in col1, e2 in col2, ...) statement
```

In the syntax above, `col2` is fully iterated for each `e1` and so on, so supplying multiple `in` declarations effectively turns the For into **nested iteration**.

For now, we only define iteration for **elements in lists and maps**. Ultimately, we want any type defining a monadic `flatMap` to be iterable with a For expression.

As we don't support pattern matching yet, **map iteration** looks like this:

```
for (kv in m) {
  const key = get(kv, 0)
  const value = get(kv, 1)
}
```

To iterate over a list of indices, you can use a **range** function. Conceptually, it creates a *lazily* evaluated list of indices. We might ultimately support ranges with prettier operators.

```
for (i in range(0, 10)) { // 0 inclusive, 10 exclusive
  println(i)
}
```

Internally, we can of course replace the range construction with a standard index-increment for-loop. Using a range is certainly more concise and clearer than writing `for (const i = 0; i < 10; i += 1)`. Not that that's Lore code, but it *could* be. *Shudder.*

##### Yield

**TODO:** It's quite strange that Lore wants to tend towards functional/monadic handling of collections, but then provides such an imperative way of building the result of a loop. Wouldn't it rather be better if we just put the result of the loop in a list? And then couldn't we just flatten the list afterward?

All repetitions return a list of values. When a loop is entered, conceptually, we create an empty list to which elements are supposed to be appended. The **yield expression** is the way to append a value to that list. The yield expression itself evaluates to `()`. It does *not* interrupt program flow, that is, you can yield twice or more in the same iteration.

```
const names = for (animal in animals) yield animal.name
```

If `yield` is used, the Lore compiler requires that the value returned by the repetition expression is used, i.e. either assigned to a variable or enclosed in a larger expression.

In the actual implementation, we can of course **optimize** two cases:

- When the value of a repetition isn't assigned or used, we can forgo creating and filling a list.
- If there are no `yield` expressions within the block of a repetition, we can simply designate the empty list as the result.

A yield is a **top-level expression**. If no yields are present, the loop evaluates to the unit tuple.



### Variable Declarations, Assignments, and Expressions

A **variable declaration** lets you define a new variable, while an **assignment** lets you assign a value to a variable or property. Both are **top-level expressions**. A **variable expression** is simply an expression that evaluates to the value of its mentioned variable.

Here is the **syntax** of declarations, assignments, and variable expressions:

```
const x: T = v1  // immutable variable declaration
let x: T = v1    // mutable variable declaration
x = v2           // variable assignment (only valid if x is mutable)
x				 // evaluates to v2
```

A **declaration** creates a new variable. The type of the variable will be inferred from the assignment, but you can specify the type manually. For now, values need to be manually assigned to declared variables, even if they are desired to be `0`, `''`, `[]`, etc. That is, there are no default assignments. In the future, we can introduce a multi-function `lore.default` (dispatching on some kind of type object, maybe `Type[T]`) that returns a default value for a type (`lore` would be the namespace of this function, which would be the namespace for special language definitions).

Variables can be **mutable or immutable**. Only mutable variables can be assigned to after setting their value with the initial declaration. We recommend to declare all variables as immutable unless mutability is specifically needed.

An **assignment** overwrites the value of a variable with some new value. The value on the right side must be compatible with the type of the variable. The name on the left side may be a variable or a property:

```
const x = 0.0
x = 5                        // Variable assignment
character.name = 'Weislaus'  // Property assignment
character.Position.x = x     // Deep property assignment
```

Both declarations and assignments are technically expressions and they return the **unit type**. However, there is an additional restriction for declarations and assignments: They can only stand in specific places in the program, and never embedded within most expressions. This ensures that the order of execution for assignments is well defined. An assignment will never be able to mingle with an expression that uses the newly assigned value on the same line.

Lore offers the following **assignment shorthands:**

```
a += b  // a = a + b
a -= b  // a = a - b
a *= b  // a = a * b
a /= b  // a = a / b
```



### Return

The **return** *statement* returns a value from a function. The syntax is `return expr`, with `expr` evaluating to the value to return. Use return only if you 'desperately' need to return early from a function. Prefer *using blocks and control structures as expressions*.



### Operator Precedence

Our **operator precedence** is as follows, from lowest to highest precedence:

```
|
&
== =/=
< <= > >=
+ -
* /
~
atoms (including function application)
```

Note that we don't view assignments as operators. **Complex expressions** such as conditionals and blocks cannot stand as an operand; you will have to enclose them in parentheses to use them with addition, for example:

```
5 + ({ 10 }) + (if (a == b) 5 else 15)
```

