# Expressions

This document outlines all valid **expressions** found in Lore.

**TODO:** How do we implement type casts / conversions?



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

**TODO:** We could consider, once we have introduced nullable/optional values (`Int?`), to turn the logical operators into operators that accept any argument types and return "truthy" values.

##### Equality and Order

True is equal to true, false is equal to false. Booleans are not ordered.



### Strings

Lore supports UTF-8 **strings**. Their type is `String`. We will implement strings using the standard Javascript string type. Javascript's string functions will *not* be available by default; instead, Lore will define its own functions.

Conceptually, a string is *not* a list of characters. **A string is just a string.** If you access a single character at a specific index either with iteration or the `character` function, you will get another string. We believe this is a more unified framework than adding a type just for characters. (Not to mention that, if we called a type `Character`, we'd clash with game developers who might want to have a type `Character`. We'll implement namespaces, of course, but even then, it'll be useful not to have this type name reserved.)

A string is always written within **single quotes**: `'text'`. We reserve the ability to use the double quotes symbol for string-related features later on or something else entirely.

Strings are **interpolated** by default. You will be able to use `${expr}` for complex expressions, but for now `$s`, the shorthand for single variables, is the only way to interpolate strings.

The following **escaped characters** are available: `\n`, `\t`, `\'`, `\$`.

We will add **multi-line strings** in another version of Lore.

###### Example

What's possible now:

```
const k = 10
const p: Person = ...
const name = p.name
const price = if (k < 10) 'free' else '1000\$'
const announcement = '$name, you have $k apples. Please claim your $price apple at the reception.'
```

Once we allow interpolating complex expressions, this will be possible:

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

**TODO:** How can we define `get` such that it supports tuples of arbitrary length? Give every tuple type a supertype called `Product` and implement `get` via multiple dispatch?

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

Lore supports **object instantiation**, i.e. the construction of an object value given a class constructor.

**TODO:** Write this once we have written the classes and components spec document.

##### Equality and Order

Object equality is defined as **referential equality** by default.



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



### Variable Assignments

**Note:** Assignments return the Unit type.



### Return

The **return** *statement* returns a value from a function. We admit, even though we managed to turn even variable assignments into expressions (without making it messy, like in C), there is just no way a special control statement like return could be an expression. The syntax is simply `return expr`, with expr simply evaluating to the value to return.

**Note:** Use return only if you 'desperately' need to return early from a function. Prefer *using blocks and control structures as expressions*.