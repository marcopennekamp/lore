# Expressions

Lore is an expression-based language, which means that there are no statements, only expressions. This document presents the kinds of **values and expressions** available in Lore.

**TODO:** How do we implement type casts / conversions? (Typings / Type Assertions: `expr :: Type`.)



### Top-Level Expressions

**Top-level expressions** are a special type of expression which may only appear at the top level of blocks and in the bodies of conditional and loop expressions.

###### Syntax Example

```
// They are legal at the top level of blocks.
do let x = 0 end

// They are legal as the body of conditionals and loops.
if y == 0 then x = 0 else x = 5
while i < 10 yield i += 1
for entity <- entities yield count += 1
```

Top-level expressions are currently **variable declarations**, **assignments**, and **returns**.



### Variable Declarations and Expressions

A **variable declaration** is a top-level expression that lets you define a new variable. The type of the variable will be inferred from the assignment, but you can specify the type manually. Values need to be explicitly assigned to declared variables, even if they are desired to be `0`, `''`, `[]`, etc. We believe in the value of explicitness when it comes to "default" values.

Variables can be **immutable or mutable**. Only mutable variables can be changed after their initial declaration. We recommend to declare all variables as immutable unless mutability is specifically needed. This is also one reason why the mutability syntax is relatively verbose.

A **variable expression** is an expression that evaluates to the value of its named variable. A variable expression may also evaluate to the function value of a multi-function with the same name.

Variable **identifiers** may be a combination of letters, numbers, underscores, and question marks.

###### Syntax Example

```
let x: T = v1      // immutable variable declaration
let x = v1         // inferred immutable variable declaration
let mut x: T = v1  // mutable variable declaration
let mut x = v1     // inferred mutable variable declaration
x				   // variable expression
```



### Assignments

An **assignment** lets you assign a new value to a mutable variable or property. The type of the right-side value must be compatible with the type of the variable or property. Assignment is a **top-level expression** that evaluates to unit.

###### Syntax Example

```
x = 5                        // variable assignment, only valid if `x` is mutable
character.name = 'Weislaus'  // property assignment, `name` must be mutable
character.position.x = x     // deep property assignment, only `x` must be mutable
```

##### Shorthands

Lore offers the following **assignment shorthands:**

```
a += b  // a = a + b
a -= b  // a = a - b
a *= b  // a = a * b
a /= b  // a = a / b
```



### Return

The **return** top-level expression returns a value from a function. The syntax is `return expr`, with `expr` evaluating to the returned value. Use return only if you 'desperately' need to return early from a function. Prefer using blocks and control structures as expressions. Return evaluates to the `Nothing` type, since it interrupts code execution.

###### Syntax Example

**Early returns** are a useful way to achieve cleaner code:

```
func foo(x: Int): String = do
  if bar(x) then return 'cool'
  if baz(x + 2) then return 'cruel'
  'ouch!'
end
```

##### Nesting Returns

Returns cannot be nested in top-level expressions that are not at the **top-level of a function**. For example, the following code is *illegal*:

```
func foo(): String = do
  if (do return false end) then 'hello' else 'world'
end
```

Lore's semantics would not be well defined if we allowed such constellations.



### Blocks

A **block** is a sequence of expressions, the last of which is what the block evaluates to. *Blocks are expressions*. You can write code like this:

```
let result = do
  let a = 5
  let b = 10.0
  let c = get_reason()
  if c == 'business' then a * b else a / b
end
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

For now, we want to keep literal grammar to a minimum. Hence, we do not support scientific notation and only support decimal numbers. Here are the **valid number formats:**

- **Integers:** `x` or `-x`, x being any number from 0 to MAX_SAFE_INTEGER.
- **Reals:** `x.y` or `-x.y`, with both x and y being numbers. We do not allow notations such as `.0` or `1.`.

##### Arithmetic Operators

The following **arithmetic operators** can be used on numbers. Note that the remainder operator is implemented as a function.

```
a + b  // Addition
a - b  // Subtraction
a * b  // Multiplication
a / b  // Division
-a     // Negation
```

Arithmetic operations have the following **typing rules:**

- If both arguments are real or integer, the result is also **of the same type**.
- If one of the arguments is real, the result will also **be real**.

This only concerns types at **compile time**, of course. A calculation such as `2.5 * 2` might be typed as `Real` at compile-time, but provides a run-time value of `Int`. This is not a problem, since `Int` is a subtype of `Real`. This cannot happen in reverse, either: two integers will always produce another integer, unless the result exceeds (or would exceed) the MAX_SAFE_INTEGER or MIN_SAFE_INTEGER limit.

##### Equality and Order

Numbers are equal and ordered in accordance with the rules of sanity. Integers and reals can be mixed in comparisons without any issues.



### Booleans

Lore supports **booleans**. Their type is `Boolean`. They are implemented using the standard Javascript boolean type. There are two boolean **values:** `true` and `false`. 

##### Logical Operators

The following **logical operators** can be used on booleans. All arguments have to be `Boolean` values.

```
a && b  // Conjunction
a || b  // Disjunction
!a      // Logical Not
```

##### Equality and Order

True is equal to true, false is equal to false. Booleans are unordered.



### Strings

Lore supports UTF-8 **strings**. Their type is `String`. Strings are implemented using the standard Javascript string type. Javascript's string functions will *not* be available by default; instead, Lore will define its own functions.

Conceptually, a string is *not* a list of characters. **A string is just a string.** If you access a single character at a specific index either with iteration or the `character` function, you will get another string. We believe this is a more unified framework than adding a type just for characters.

A string is always written within **single quotes**: `'text'`. We reserve the ability to use the double quotes symbol for string-related features later on or something else entirely.

Strings are **interpolated** by default. You can use `$e` for simple expressions and `${expr}` for complex ones.

The following **escaped characters** are available: `\n`, `\r`, `\t`, `\'`, `\$`, `\\`, as well as Unicode escapes such as `\u0008`.

We will add **multi-line strings** in another version of Lore.

###### Example

```
let k = 10
let p = Person('Smith Johnson', 48)
let announcement = '${p.name}, you have $k apples. Please claim your ${if k < 10 then 'free' else '1000\$'} apple at the reception.'
```

##### String Operators

One might expect the plus operator to support **string concatenation**. This is not the case in Lore. In most cases, *interpolation* will be the preferable option compared to operative concatenation. In all other cases, most likely when you're working algorithmically with strings, concatenation is provided as a function `concat`.

(**Note:** This will obviously change when we introduce user-defined operators.)

##### Equality and Order

Two strings are equal if they have the same length and characters. Strings are ordered alphabetically.



### Tuples

Lore supports **tuples**. As described by tuple types, tuples are fixed-size, heterogenous lists of values. Tuples are simply created by putting parentheses around comma-separated values: `(a, b, c)`. A tuple value's type is the tuple type of the respective element types.

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



### Anonymous Functions

In addition to multi-functions, Lore supports **anonymous functions**. An anonymous function is created as an immediate function value without dispatch mechanics. Parameter types may be specified optionally, but can also be inferred from *local* context. The return type of the anonymous function is always inferred.

**TODO:** How should we handle returns in anonymous functions?

###### Example

```
let square: Real => Real = v => v * v
map([1, 2, 3, 4, 5], v => v + 3)
```



### Lists

Lore supports **lists** as first-class constructs. A list is a homogeneous, linear collection of an arbitrary number of elements. Lists are *immutable*. List types are denoted `[A]`. We will eventually differentiate between immutable lists and (mutable) arrays.

You can **construct** a list by putting comma-separated elements inside square brackets: `[a, b, c]`. The empty list is denoted simply `[]`. You can **append** to a list with the `:+` operator, which is the native way to expand a list.

##### Equality and Order

Two lists are equal if they have the same lengths and each of their elements, considered in order, are equal. Lists are unordered by default.



### Maps

Lore supports **maps** as first-class constructs. A map is a homogeneous, indexed collection of key/value pairs. Maps are *immutable*. Map types are denoted `#[A -> B]`. We will eventually differentiate between immutable and mutable maps.

You can **construct** a map with the following syntax: `#[k1 -> v1, k2 -> v2, k3 -> v3]`. The empty map is denoted `#[]`.

**TODO:** Appending to a map?

###### Example

We can define a map from strings to integers:

```
let points = #['Ameela' -> 120, 'Bart' -> 14, 'Morrigan' -> 50]
// points: #[String -> Int]
```

##### Equality and Order

Two maps are equal if for each key/value pair in the first map, there is a key/value pair in the second map, and vice versa. Maps are unordered by default.



### Shapes

**Shapes** are first-class values. Refer to [structs, traits, and shapes]() for more information.

###### Example

```
let bark_options = %{ show_teeth: true, volume: 80 }
// bark_options: %{ show_teeth: Boolean, volume: Int }
```

##### Equality and Order

Two shapes are equal if their properties are equal. Shapes are unordered by default.



### Symbols

A **symbol** is a value simply identified and typed by its name. A symbol named `foo` is written `#foo` and its type is `#foo`. Symbols are interned at run time.

We suggest using a snake_case naming convention for symbols.

###### Example

```
func process(query: Query): Result | #syntax_error = do
  let parsed = parse(query)
  if error?(parsed) then #syntax_error
  else get_result(parsed)
end
```

##### Equality and Order

Two symbols are equal if they have the same name. Symbols are unordered.



### Structs

Lore supports **struct instantiation**. There are two possible syntax flavors:

```
struct A(b: B)

let b = B()
let a = A(b)        // Call syntax
let a = A { b: b }  // Map syntax
let a = A { b }     // Map syntax using shorthand
```

The call-syntax constructor is an ordinary **function value** and can be used as such:

```
func construct(f: B => A, b: B): A = f(b)

let b = B()
let a = construct(A, b)
```

A **type alias** also defines a corresponding constructor function value:

```
struct Box[+A](value: A)
type StringBox = Box[String]

let box = StringBox('I am in a box.')
```

##### Equality and Order

Struct equality is defined as **referential equality** by default. (**TODO:** Really? Not very useful. Structs should have some default notion of equality.)



### Member Access

You can access a **member** of a value with the `.` notation. The type of the expression is the type of the member. The syntax is simply: `value.member`.



### Comparison Operators

Lore supports the following **comparison operators:**

```
a == b   // Equality
a != b   // Inequality
a < b    // Less than
a <= b   // Less than or equal
a > b    // Greater than
a >= b   // Greater than or equal
```

To **define equality** for a given type, you can specialize the function `equal?(a, b)`. Inequality is strictly defined as `!equal?(a, b)`.

```
func equal?(c1: Car, c2: Car): Boolean = ...
func equal?(SportsCar, CheapCar): Boolean = false
func equal?(CheapCar, SportsCar): Boolean = false // Don't forget to be symmetric!
```

To **define order** for a given type, specialize the function `less_than?(a, b)` and optionally `less_than_equal?(a, b)`. The latter is already defined as `less_than?(a, b) || equal?(a, b)` in Pyramid. Greater than, `a > b`, is strictly defined as `b < a`, and `b >= a` as `a <= b` .



### Function Calls

The syntax of a **function call** is simple:

```
target(a1, a2, ...)
```

**Call semantics** depend on the `target`:

- If the target is a **multi-function**, the compiler will simulate multiple dispatch to find the correct function implementation. The semantics of such multi-function calls are defined in [multi-functions](multi-functions.md).

  We say *multi-function call*, because it only becomes a function call at run-time, when a function has been chosen according to the dispatch semantics. At compile-time, we are calling a whole multi-function with a bounded but unknown input type.

- If the target is a **function value**, the function will be called directly at run time. Anonymous functions and constructors will be called directly, but if the function value refers to a multi-function, multiple dispatch will of course still be performed.

##### Pipes

Function application may be chained using **pipes**:

```
a1 |> target(a2, ...)
```

Pipes are a purely syntactic construct. They are transformed to equivalent nested function calls. For example, the pipe application above would be transformed to `target(a1, a2, ...)`.

###### Example

```
['Hello', 'Bonjour', 'Hola', 'Privyet', 'Ciao', 'Hallo', 'Hej']
  |> filter(str => String.length(str) < 5)
  |> map(str => '$str, world!')
  |> String.join(' ')
```

This example will be transformed to:

```
String.join(
  map(
    filter(
      ['Hello', 'Bonjour', 'Hola', 'Privyet', 'Ciao', 'Hallo', 'Hej'],
      str => String.length(str) < 5
    ),
    str => '$str, world!'
  ),
  ' '
)
```



### Fixing Functions at Compile-Time

Instead of deciding dispatch at run-time, you can **fix a function at compile-time:**

```
f.fixed[T1, T2, ...]
```

The expression evaluates to a **function value** which may be subsequently invoked or passed around.



### Dynamic Function Calls

Many functions in the Lore standard library, especially the most fundamental ones, defer their implementation to **dynamic function calls**. Since the underlying runtime is a Javascript environment, when defining a function implementation, we can also call functions defined in Javascript.

The **syntax** of a dynamic function call is as follows:

```
dynamic[ResultType]('f', a1, a2, ...)
```

The first argument is the name of the function to call. For now, the Lore compiler will **trust** the programmer that, when named, such a function is actually available, takes the given arguments, and returns a value that conforms to the required type bounds. The name of the function doesn't have to be a simple name, but may be valid Javascript, such as accessing a namespace: `'Lore.list.push'`. However, the name is required to be a string literal (that doesn't include interpolation).

In the long term, we want to be able to use, for example, TypeScript declaration files to provide more **type safety** when interfacing with Javascript or TypeScript code. Also note that, as of now, special Lore objects are passed to a native function, which are Javascript values additionally wrapped in an object to provide type information at run-time. Only primitives such as numbers and strings are unwrapped. A dynamic function will have to deal with these idiosyncrasies until we have come up with a native unwrapping solution.



### Conditional Expressions

Lore will support a variety of **conditional expressions** (especially pattern matching, guards, and so on). For now, we will have to make do with If:

```
if cond then tle1 else tle2

if cond
  tles1
end

if cond
  tles1
end else
  tles2
end
```

Note that either top-level expression (TLE) (or even `cond`) may be a block. The else part is, of course, optional. The so-called **dangling else** is always parsed as belonging to the `if` closest to it.

The if-expressions without a `then` require the top-level expression(s) to be placed on the next line. This also implicitly opens a block, which must be closed with an `end`. An `else` with an implicit block must also be closed with an `end`.



### Loops

**Loops** represent repeating control flow. For now, we support while loops and for comprehensions.

##### While Loop

In Lore, a **while loop** repeats some piece of code as long as a given boolean expression is true:

```
while cond yield tle

while cond
  tles
end
```

We have decided to provide **no support for do-while loops**, because we feel that these kinds of loops are very rarely used, but add noise to the language in the form of an additional keyword being reserved (such as `repeat`). You should instead work with a function and a while or recursion. This will only become easier once we introduce anonymous functions.

##### For Comprehension

A **for comprehension** iterates over some kind of collection:

```
for e1 <- col1, e2 <- col2, ... yield tle

for e1 <- col1, e2 <- col2
  tles
end
```

In the syntax above, `col2` is fully iterated for each `e1` and so on, so supplying multiple extractors effectively turns the comprehension into **nested iteration**.

For now, we only define iteration for **lists and maps**. Ultimately, we want any type defining a monadic `flatMap` to be iterable using a for comprehension. (Or, alternatively, any type implementing an enumerable-like interface.)

As we don't support pattern matching yet, **map iteration** looks like this:

```
for kv <- m              // kv is a tuple
  let key = first(kv)
  let value = second(kv)
end
```

##### Loop Expressions

Loops are expressions, too. Similar to if-expressions and blocks, the loop body expression determines the result of the loop. However, these evaluations are **aggregated into a list**.

Take the following **example:**

```
let names = for animal <- animals yield animal.name
```

The for comprehension **aggregates the names** of all animals in a list of type `[String]`. 

In the implementation, we can of course **optimize** the following case: When the result of a loop isn't assigned or used, we can forgo creating and filling the list.



### Operator Precedence

Our **operator precedence** is as follows, from lowest to highest precedence:

```
||
&&
== !=
< <= > >=
+ -
* /
! - (unary)
atoms (including function application)
```

Note that we don't view assignments as operators. **Complex expressions** such as conditionals cannot stand as an operand; you will have to enclose them in parentheses to use them with addition, for example:

```
5 + (do 10 end) + (if a == b then 5 else 15)
```



### Post-MVL Extensions (Ideas)

- Consider introducing **Swift-style `guard` statements** with a twist: They operate within blocks. If the condition is false, continue the code, otherwise *return the value of the else part from the block*. I think this could be super useful in game development.

- We could consider, once we have introduced Options, to **turn the logical operators into operators that accept any argument types** and return "truthy" values. Compare to Clojure, Elixir, or Javascript.

- **Lists:**

  - Define a default **order** for lists, which would probably make programmer life easier.

- **Tuples:**

  - How can we define `get` such that it **supports tuples of arbitrary length**? Give every tuple type a supertype called `Tuple` and implement `get` via multiple dispatch?
  - Implement some default tuple **ordering**.

- A feature such as Swift's **trailing closures**, maybe add some way to pass two or more closures. (Or just multiple parameter lists.)

- Add a match-like expression that executes the **first case where a boolean expression is true:**

  ```
  cond
    foo(x, y) => 'hello'
    bar(x, y) => 'world'
    baz(x, y) && baz(y, x) && baz(z, z) => 'hello darkness'
    _ => 'default'
  end
  ```

- **If-else:**

  - If only **one branch** of an if-else expression supplies a value, return an **option** of the evaluated type.