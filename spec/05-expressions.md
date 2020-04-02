# Expressions

This document outlines all valid **expressions** found in Lore.

**TODO:** How do we implement type casts / conversions?



### Literals & Value Constructors

*On the first day, there was the **value**.* We cannot define complex expressions without having values first. We also consider their types, if they haven't been defined yet.

##### Numbers

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



##### Booleans

**TODO:** Long-term, we could define booleans within Lore as singleton types.

**TODO:** We could make `Boolean` abstract, introduce two subtypes `True` and `False`, and allow dispatching on boolean "types". This is definitely ~~a whacky~~ an idea, and either very cool or very stupid. One problem I see is that when a boolean could be dispatched on as a flag, what we really want is a label type: `function contains(list: [a], value: a, isSorted: Boolean)` and `function contains(list: [a], value: a, isSorted: True)` etc. compared to `function contains(list: [a], value: a)` and `function contains(list: [a] & Sorted, value: a)`.

Lore supports **booleans**. Their type is `Boolean`. We will implement them using the standard Javascript boolean type. There are two boolean **values:** `true` and `false`. 



##### Strings

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



##### Tuples

Lore supports **tuples**. As described by product types, tuples are fixed-size, heterogenous lists of values. Tuples are simply created by putting parentheses around comma-separated values: `(a, b, c)`. A tuple value's type is the product type of the respective element types.

###### Example

**TODO:** How can we define `get` such that it supports tuples of arbitrary length? Give every tuple type a supertype called `Product` and implement `get` via multiple dispatch?

```
const t = (a, b, c)
get(t, 0) // a
get(t, 2) // c
```

