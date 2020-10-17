# Expressions

Lore is an expression-based language, which means that there are no statements, only expressions. This document presents the kinds of **values and expressions** available in Lore.

**TODO:** How do we implement type casts / conversions? (Typings / Type Assertions: `expr :: Type`.)



### Top-Level Expressions

**Top-level expressions** are a special type of expression which may only appear at the top level of blocks and in the bodies of conditional and loop expressions.

###### Syntax Example

```
// They are legal at the top level of blocks.
{ let x = 0 }

// They are legal as the body of conditionals and loops.
if (y == 0) x = 0 else x = 5
repeat while (i < 10) i = i + 1
for (entity in entities) count = count + 1
```

Top-level expressions are currently **variable declarations**, **assignments**, and **returns**.



### Variable Declarations, Assignments, and Expressions

A **variable declaration** lets you define a new variable, while an **assignment** lets you assign a value to a variable or property. Both are **top-level expressions**. A **variable expression** is simply an expression that evaluates to the value of its mentioned variable.

Here is the **syntax** of declarations, assignments, and variable expressions:

```
let x: T = v1      // immutable variable declaration
let mut x: T = v1  // mutable variable declaration
x = v2             // variable assignment (only valid if x is mutable)
x				   // evaluates to v2
```

A **declaration** creates a new variable. The type of the variable will be inferred from the assignment, but you can specify the type manually. Values need to be manually assigned to declared variables, even if they are desired to be `0`, `''`, `[]`, etc. We believe in being explicit, especially since most variables should be constants anyway.

Variables can be **mutable or immutable**. Only mutable variables can be assigned to after setting their value with the initial declaration. We recommend to declare all variables as immutable unless mutability is specifically needed. This is also the reason why the mutability syntax is relatively verbose.

An **assignment** overwrites the value of a variable or property with some new value. The value on the right side must be compatible with the type of the variable or property. The name on the left side may be a variable or a property:

```
let mut x = 0.0
x = 5                        // Variable assignment
character.name = 'Weislaus'  // Property assignment
character.Position.x = x     // Deep property assignment
```

Both declarations and assignments are technically expressions and they return the **unit type**. This result is of course mostly useless.

Lore offers the following **assignment shorthands:**

```
a += b  // a = a + b
a -= b  // a = a - b
a *= b  // a = a * b
a /= b  // a = a / b
```



### Return

The **return** top-level expression returns a value from a function. The syntax is `return expr`, with `expr` evaluating to the value to return. Use return only if you 'desperately' need to return early from a function. Prefer *using blocks and control structures as expressions*. Return evaluates to the `Nothing` type, since it interrupts code execution.

###### Syntax Example

**Early returns** are a useful way to achieve cleaner code:

```
function foo(x: Int): String = {
  if (bar(x)) return 'cool'
  if (baz(x + 2)) return 'cruel'
  'ouch!'
}
```

##### Nesting Returns

Returns **cannot be nested** in top-level expressions that are not at the top-level of a function. For example, the following code is illegal:

```
function foo(): String = {
  if ({ return false }) 'hello' else 'world'
}
```

Lore's semantics would not be well defined if we allowed such constellations.



### Blocks

A **block** is a sequence of expressions, the last of which becomes the value of the block. *Blocks are expressions*. You can write code like this:

```
let result = {
  let a = 5
  let b = 10.0
  let c = getReason()
  if (c == 'business') a * b else a / b
}
```

Blocks also give you the luxury of **lexical scoping**, so make sure you declare variables exactly where you need them. In the example above, neither a, b, nor c are visible outside the block.



### Numbers

Lore supports **real** and **integer** numbers. Their types are `Real` and `Int`. `Int` is a subtype of `Real`. They are both implemented using the standard Javascript number type, and so precise integers can only be guaranteed up to [MAX_SAFE_INTEGER](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Number/MAX_SAFE_INTEGER). We believe this is sufficient for all common use cases.

We are not introducing integers for performance reasons. Rather, we think that it is useful to **state intent with the type:** if a function expects a `Real` number, it can be any number, while a function expecting an `Int` is explicitly requiring the number to be an integer.

Internally, integers are represented as reals, but with additional **guarantees and checks:**

- The compiler guarantees that integer **literals** have `Int` shape.
- The compiler guarantees that integer **operations** evaluate to `Int`.
- The compiler guarantees that functions **returning** `Int` have integer shape.
- **Reals** cannot be passed as integers and need to be manually cast down.
  - Since there are no precision issues, there is no need to require manual casting of integers to reals.
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
- **TODO:** This is wrong. Calculations like `2.5 * 2` don't necessarily yield a `Real`, since `Int` is decided at run-time.

##### Equality and Order

Numbers are equal and ordered in accordance with the rules of sanity. Integers and reals can be mixed in comparisons without any issues.



### Booleans

Lore supports **booleans**. Their type is `Boolean`. They are implemented using the standard Javascript boolean type. There are two boolean **values:** `true` and `false`. 

##### Logical Operators

The following **logical operators** can be used on booleans. All arguments have to be `Boolean` values.

```
a & b  // Conjunction
a | b  // Disjunction
~a     // Logical Not
```

We define *logical not* using a **tilde** so we can reserve the `?!` combo of characters for handling optional values.

##### Equality and Order

True is equal to true, false is equal to false. Booleans are unordered.



### Strings

Lore supports UTF-8 **strings**. Their type is `String`. We will implement strings using the standard Javascript string type. Javascript's string functions will *not* be available by default; instead, Lore will define its own functions.

Conceptually, a string is *not* a list of characters. **A string is just a string.** If you access a single character at a specific index either with iteration or the `character` function, you will get another string. We believe this is a more unified framework than adding a type just for characters.

A string is always written within **single quotes**: `'text'`. We reserve the ability to use the double quotes symbol for string-related features later on or something else entirely.

Strings are **interpolated** by default. You can use `$e` for simple expressions and `${expr}` for complex ones.

The following **escaped characters** are available: `\n`, `\r`, `\t`, `\'`, `\$`, `\\`, as well as Unicode escapes such as `\u0008`.

We will add **multi-line strings** in another version of Lore.

###### Example

```
let k = 10
let p = Person('Smith Johnson', 48)
let announcement = '${p.name}, you have $k apples. Please claim your ${if (k < 10) 'free' else '1000\$'} apple at the reception.'
```

##### String Operators

One might expect the plus operator to support **string concatenation**. This is not the case in Lore. In most cases, *interpolation* will be the preferable option compared to operative concatenation. In all other cases, most likely when you're working algorithmically with strings, concatenation is provided as a function.

(**Note:** This will obviously change when we introduce user-defined operators.)

##### Equality and Order

Two strings are equal if they have the same length and characters. Strings are ordered alphabetically.



### Tuples

Lore supports **tuples**. As described by product types, tuples are fixed-size, heterogenous lists of values. Tuples are simply created by putting parentheses around comma-separated values: `(a, b, c)`. A tuple value's type is the product type of the respective element types.

###### Example

```
let t = (a, b, c)
get(t, 0) // a
get(t, 2) // c
```

##### Unit

Lore supports a **unit** value, which is simply the empty tuple. It is written `()` and has the type `Unit` or `()`. The unit value is special in Lore, as it is the de-facto throwaway value, and also the implicit return type of actions.

##### Equality and Order

Two tuples are equal if they have the same size and their elements are equal.



### Lists

Lore supports **lists** as first-class constructs. A list is a homogenous, linear collection of an arbitrary number of elements. Lists are *immutable*. List types are denoted `[A]`. We will eventually differentiate between immutable lists and (mutable) arrays.

You can **construct** a list by putting comma-separated elements inside square brackets: `[a, b, c]`. The empty list is denoted simply `[]`. You can **append** to a list with the `:+` operator, which is the native way to expand a list.

##### Equality and Order

Two lists are equal if they have the same lengths and each of their elements, considered in order, are equal. Lists are not ordered by default.



### Maps

Lore supports **maps** as first-class constructs. A map is a homogenous, indexed collection of key/value pairs. Maps are *immutable*. Map types are denoted `A -> B`. We will eventually differentiate between immutable and mutable maps.

You can **construct** a map with the following syntax: `%{ k1 -> v1, k2 -> v2, k3 -> v3 }`. The empty map is denoted `%{ }`.

**TODO:** Appending to a map?

###### Example

We can define a map from strings to integers:

```
let points = %{ 'Ameela' -> 120, 'Bart' -> 14, 'Morrigan' -> 50 }
// points: String -> Int
```

##### Equality and Order

Two maps are equal if for each key/value pair in the first map, there is a key/value pair in the second map, and vice versa. Maps don't define an order by default.



### Objects

Lore supports **object construction**, i.e. the instantiation of an object given a struct constructor and the required arguments. There are two possible syntax flavors:

```
// Assuming another value b: B
let a = A(b)        // Call syntax
let a = A { b: b }  // Map syntax
let a = A { b }     // Map syntax with shorthand
```

##### Equality and Order

Object equality is defined as **referential equality** by default.



### Member Access

You can access the **member** of an object with the `.` notation. The type of the expression is the type of the member. The syntax is simply: `object.member`.

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

**TODO:** Rethink that "not equals" operator…



### Multi-Function Calls

**Multi-Function calls** are the heart of Lore. Their syntax is simple:

```
name(a1, a2, ...)
```

That's it! Types will be checked, values will be dispatched, and some function will be called. The **semantics** of multi-function calls are defined in [multi-functions](multi-functions.md).

We say *multi-function call*, because it only becomes a function call at run-time, when a function has been chosen according to the dispatch semantics. At compile-time, we are calling a whole multi-function with a bounded but unknown input type.

##### Fixed Invocation

Instead of dispatching at run-time, you can **fix a function to call at compile-time:**

```
f.fixed[T1, T2, ...](a1, a2, ...)
```

In the long term, `f.fixed[T1, T2, ...]` is supposed to return a function value. As we are not supporting these for now, the whole syntax is parsed and compiled as one big expression.



### Dynamic Function Calls

Many functions in the Lore standard library, especially the most fundamental ones, defer their implementation to **dynamic function calls**. Since the underlying runtime environment is a Javascript environment, when defining a function implementation, we can also call functions defined in Javascript.

The **syntax** of a dynamic function call is as follows:

```
dynamic[ResultType]('f', a1, a2, ...)
```

For now, the Lore compiler will **trust** the programmer that, when named, such a function is actually available, takes the given arguments, and returns a value that conforms to the required type bounds. The name of the function doesn't have to be a simple name, but may be valid Javascript, such as accessing a namespace: `'Lore.list.push'`. However, the name is required to be a string literal (that doesn't include interpolation).

In the long term, we want to be able to use, for example, TypeScript declaration files to provide more **type safety** when interfacing with Javascript or TypeScript code. Also note that, as of now, special Lore objects are passed to a native function, which are Javascript values additionally wrapped in an object to provide type information at run-time. Only primitives such as numbers and strings are unwrapped. A dynamic function will have to deal with these idiosyncrasies until we have come up with a native unwrapping solution.



### Conditional Expressions

Lore will support a variety of **conditional expressions** (especially pattern matching, guards, and so on). For now, we will have to make do with If:

```
if (cond) statement2 else statement2
```

Note that either `statement` (or even `cond`) may be a block. The else part is, of course, optional. The so-called **dangling else** is always parsed as belonging to the `if` closest to it.



### Loops

**Loops** represent repeating control flow. For now, we support while loops and for comprehensions.

##### While Loop

In Lore, a **while loop** repeats some piece of code as long as a given boolean expression is true:

```
while (cond) statement
```

We have decided to provide **no support for do-while loops**, because we feel that these kinds of loops are very rarely used, but add noise to the language in the form of an additional keyword being reserved (such as `do` or `repeat`). You should instead work with a function and a while or recursion. This will only become easier once we introduce anonymous functions.

##### For Comprehension

A **for comprehension** iterates over some kind of collection:

```
for (e1 <- col1, e2 <- col2, ...) statement
```

In the syntax above, `col2` is fully iterated for each `e1` and so on, so supplying multiple extractors effectively turns the comprehension into **nested iteration**.

For now, we only define iteration for **elements in lists and maps**. Ultimately, we want any type defining a monadic `flatMap` to be iterable using a for comprehension.

As we don't support pattern matching yet, **map iteration** looks like this:

```
for (kv <- m) {
  let key = first(kv)
  let value = second(kv)
}
```

To iterate over a list of indices, you can use a **range** function. Conceptually, it creates a *lazily* evaluated list of indices. We might ultimately support ranges with prettier operators.

```
for (i <- range(0, 10)) { // 0 inclusive, 10 exclusive
  println(i)
}
```

Internally, we can of course replace the range construction with a standard index-increment for-loop. Using a range is certainly more concise and clearer than writing `for (let i = 0; i < 10; i += 1)`.

##### Loop Expressions

Loops are expressions, too. Similar to if-expressions and blocks, the loop body expression determines the result of the loop. However, these evaluations are **aggregated into a list**.

Take the following **example:**

```
let names = for (animal <- animals) animal.name
```

The for comprehension **aggregates the names** of all animals in a list of type `[String]`. 

In the implementation, we can of course **optimize** the following case: When the result of a loop isn't assigned or used, we can forgo creating and filling the list.



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

**TODO:** Is this still true?



### Post-MVL Extensions (Ideas)

- Consider introducing **Swift-style `guard` statements** with a twist: They operate within blocks. If the condition is false, continue the code, otherwise *return the value of the else part from the block*. I think this could be super useful in game development.

- We could consider, once we have introduced nullable/optional values (`Int?`), to **turn the logical operators into operators that accept any argument types** and return "truthy" values. Compare to Clojure, Elixir, or Javascript.

- **Lists:**

  - Define a default **order** for lists, which would probably make programmer life easier.

- **Tuples:**

  - How can we define `get` such that it **supports tuples of arbitrary length**? Give every product type a supertype called `Product` and implement `get` via multiple dispatch?
  - Implement some default tuple **ordering**.

- A feature such as Swift's **trailing closures**, maybe add some way to pass two or more closures. (Or just multiple parameter lists.)

- Add a match-like expression that executes the **first case where a boolean expression is true:**

  ```
  ??? {
    foo(x, y) => 'hello'
    bar(x, y) => 'world'
    baz(x, y) & baz(y, x) & baz(z, z) => 'hello darkness'
    _ => 'default'
  }
  ```

- **If-else:**

  - If only **one branch** of an if-else expression supplies a value, return an **option** of the evaluated type.