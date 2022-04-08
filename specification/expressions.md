# Expressions

Lore is an expression-based language. This document presents the kinds of **values and expressions** available in Lore.



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

A **variable declaration** is a top-level expression that lets you define a new variable. The type of the variable will be inferred from the assigned value, but you can specify the type manually. Values need to be explicitly assigned to declared variables, even if they are mutable and are desired to be `0`, `''`, `[]`, etc., as Lore wants you to be explicit instead of relying on a default value.

Variables can be **immutable or mutable**. Only mutable variables can be changed after their initial declaration. We recommend declaring all variables as immutable unless mutability is specifically needed. This is also one reason why the mutability syntax is relatively verbose.

A **variable expression** is an expression that evaluates to the value of its named variable. 

See [identifiers](identifiers.md) for more information about valid and invalid variable names.

###### Syntax Example

```
let x: T = v1      // immutable variable declaration
let x = v1         // inferred immutable variable declaration
let mut x: T = v1  // mutable variable declaration
let mut x = v1     // inferred mutable variable declaration
x                  // variable expression
```



### Assignments

An **assignment** assigns a new value to a mutable variable or property. The type of the right-side value must be compatible with the type of the variable or property. Assignment is a **top-level expression** that evaluates to `Unit`.

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

The **return** top-level expression returns a value from a function. The syntax is `return expr`, with `expr` evaluating to the returned value. Use a return only if you need to return early from a function, otherwise use the fact that blocks and control structures are expressions. The return expression itself evaluates to the `Nothing` type, since it interrupts code execution.

###### Example 1

```
func contains?(strings: [String], string: String): Boolean = do
  for string2 <- strings
    if string == string2
      return true
    end
  end
  false
end
```

###### Example 2

**Early returns** are a useful way to achieve cleaner code:

```
act move(entity: Entity, distance: Int) do
  if rooted?(entity) then return
  if collision_ahead?(entity) then return  
  // Move the entity...
end
```

##### Nesting Returns

Returns **cannot be placed in nested expressions** in certain cases. For example, the following code is *illegal*:

```
func foo(): String = do
  if (do return false end) then 'hello' else 'world'
end
```

Returns may be placed in the following kinds of expressions, provided the expression itself occurs in a permissible context:

- **Blocks**
- **Variable declarations and assignments:** The right-hand value expression may contain returns.
- **`if` and `cond` expressions:** The conditions may *not* contain returns, but the bodies may.
- **`while` and `for` expressions:** The conditions/extractors may *not* contain returns, but the bodies may.

Anonymous function bodies may also not contain return expressions at this time, as the goal is to implement non-local returns. Allowing local returns now and changing their semantics later would break existing Lore code, so we are disallowing any returns inside anonymous functions. 



### Blocks

A **block** is a sequence of expressions. Blocks are expressions, as they evaluate to the value of their last expression. For example:

```
let result = do
  let a = 5
  let b = 10.0
  let c = get_reason()
  if c == 'business' then a * b else a / b
end
```

Blocks also open a new **lexical scope**. In the example above, neither `a`, `b`, or `c` are visible outside the block.



### Numbers

Lore has two distinct numeric data types, **Int** and **Real**.

For now, we want to keep the grammar of literals to a minimum. Hence, we do not support scientific notation and only support decimal numbers. Here are the **valid number formats:**

- **Int:** `x` or `-x`, x being any number from 0 to MAX_SAFE_INTEGER.
- **Real:** `x.y` or `-x.y`, with both x and y being numbers. We do not allow notations such as `.0` or `1.`.

Conversions between integers and reals can be done with the functions `lore.Int.to_real` and `lore.Real.to_int`.

##### Arithmetic Operators

The following **arithmetic operators** can be used on numbers. Note that the remainder operator is implemented as a function `lore.Math.remainder`.

```
a + b  // Addition
a - b  // Subtraction
a * b  // Multiplication
a / b  // Division
-a     // Negation
```

If `a` or `b` is a `Real` and the other is an `Int`, the `Int` will be implicitly converted to `Real`.

Division of two integers is explicitly defined as integer division, so `10 / 4` will result in `2` not `2.5`.

##### Equality and Order

Numbers are equal and ordered in accordance to mathematical norms.



### Booleans

Lore supports **booleans**. Their type is `Boolean`. There are two boolean **values:** `true` and `false`. 

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

Lore supports UTF-8 **strings**. Their type is `String`. Conceptually, a string is *not* a list of characters. Accessing a single character at a specific position or index with the `lore.String.at` functions will result in a string. 

A string is always written within single quotes: `'text'`. We reserve the ability to use the double quotes symbol for string-related features later on or something else entirely. Strings are also interpolated by default. You can use `$e` for simple expressions and `${expr}` for complex ones.

In the world of strings, the terms **index** and **position** refer to two distinct concepts. An index refers to the individual bytes in the UTF-8 string, while a position refers to a code point. The default string functions operate on code points, but can be inefficient especially when accessing code points at specific positions. Hence, there are also byte-based functions that work with indices.

The following **escaped characters** are available: `\n`, `\r`, `\t`, `\'`, `\$`, `\\`, as well as Unicode escapes such as `\u0008`.

We will add **multi-line strings** in another version of Lore.

###### Example

```
let k = 10
let p = Person('Smith Johnson', 48)
let announcement = '${p.name}, you have $k apples. Please claim your ${if k < 10 then 'free' else '1000\$'} apple at the reception.'
```

##### String Operators

One might expect the plus operator to support **string concatenation**. This is not the case in Lore. In most cases, *interpolation* will be the preferable option compared to operative concatenation. In all other cases, most likely when you're working algorithmically with strings, concatenation is provided as a function `lore.String.concat`.

##### Equality and Order

Two strings are equal if they have exactly the same bytes. Strings are ordered lexicographically by code point. This ordering will not produce good user-facing results, but constitutes a sensible default.



### Tuples

Lore supports **tuples**. As described by tuple types, tuples are fixed-size, heterogeneous lists of values. Tuples are created by putting parentheses around comma-separated values: `(a, b, c)`. The tuple's type is constructed from the element types.

###### Example

```
use lore.Tuple.get

let t = (a, b, c)
get(t, 0) // a
get(t, 2) // c
```

##### Unit

Lore supports a **unit** value, which is simply the empty tuple. It is written `()` and has the type `Unit` or `()`. The unit value is special in Lore, as it is the de-facto throwaway value, and also the implicit return type of actions.

##### Equality and Order

Two tuples are equal if they have the same size and their elements are equal under `lore.core.equal?`. A tuple is less than another tuple if they have the same size and their elements follow lexicographic ordering under `lore.core.less_than?`.



### Anonymous Functions

In addition to multi-functions, Lore supports **anonymous functions**. An anonymous function is created as an immediate function value without dispatch mechanics. Parameter types may be specified optionally, but can also be inferred from *local* context. The return type of the anonymous function is always inferred.

Anonymous functions may not contain return expressions, as noted in the section about return expressions. They will eventually be supported as non-local returns.

###### Example

```
let square: Real => Real = v => v * v
map([1, 2, 3, 4, 5], v => v + 3)
```

##### Equality and Order

Anonymous functions are equal by reference and unordered.



### Lists

Lore supports **lists** as first-class constructs. A list is an immutable, homogeneous, linear collection of an arbitrary number of elements. List types are denoted `[A]`.

You can **construct** a list by putting comma-separated elements inside square brackets: `[a, b, c]`. The empty list is denoted simply `[]` and has the type `Nothing`. You can **append** to a list with the `:+` operator, which is the native way to expand a list.

Lore currently has no native, mutable array type. They will eventually be added to Pyramid with VM support.

##### Equality and Order

Two lists are equal if they have the same lengths and each of their elements, considered in order, are equal under `lore.core.equal?`. A list `v1` is less than another list `v2` if their elements follow lexicographic ordering under `lore.core.less_than?`. If all elements are equal, but `v1` is shorter than `v2`, `v1 < v2` holds.



### Maps

Lore supports **maps** as first-class constructs. A map is an immutable, homogeneous, indexed collection of key/value pairs. Map types are denoted `#[A -> B]`. We will eventually differentiate between immutable and mutable maps.

You can **construct** a map with the following syntax: `#[k1 -> v1, k2 -> v2, k3 -> v3]`. The empty map is denoted `#[]`.

*Note:* Maps are currently unsupported and will be revised soon.

###### Example

We can define a map from strings to integers:

```
let points = #['Ameela' -> 120, 'Bart' -> 14, 'Morrigan' -> 50]
// points: #[String -> Int]
```

##### Equality and Order

Two maps are equal if they have the same size and for each key/value pair in the first map, there is a key/value pair in the second map that is equal under `lore.core.equal?`. Maps are unordered by default.



### Shapes

**Shapes** are first-class values. Refer to [shapes](shapes.md) for more information.

###### Example

```
let bark_options = %{ show_teeth: true, volume: 80 }
// bark_options: %{ show_teeth: Boolean, volume: Int }
```

##### Equality and Order

Two shapes are equal if their properties are equal under `lore.core.equal?`.

A shape `s1` is less than a shape `s2` if properties in `s1` follow lexicographic order under `lore.core.less_than?` when compared to properties with the same name from `s2`. Properties are considered in the order of their names. If `s1` contains a property that isn't contained in `s2`, `s1` cannot be less than `s2`.

Comparing shape values with the exact same property names follows a strict total order if the property values also follow a strict total order. Otherwise, shape value comparison follows a strict *partial* order, as not all elements are comparable. For example, `%{ foo: 5 }` and `%{ bar: 10 }` are incomparable, because neither value contains the other's property names.



### Symbols

A **symbol** is a value simply identified and typed by its name. A symbol named `foo` is written `#foo` and its type is `#foo`. Symbols are interned at run time.

We suggest using a `#snake_case` naming convention for symbols.

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

Lore supports **struct instantiation** using the call syntax or the map syntax:

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
let a = construct(A, b)  // Pass the constructor `A` to `construct`.
```

A **struct type alias** also defines a corresponding constructor function value:

```
struct Box[+A](value: A)
struct StringBox = Box[String]

let box = StringBox('I am in a box.')
```

##### Equality and Order

Struct equality is handled by the default implementation of `lore.core.equal?`, which compares the structs' schemas for equality and its properties with `lore.core.equal?`. You can override `lore.core.equal?` with your own definition for any combination of types.

The default implementation of struct equality considers two structs with different open property types as equal if their property values are equal under `lore.core.equal?`, as struct types have no bearing on the definition of equality.

A struct `s1` is less than a struct `s2` if `s1` and `s2` have the same schema and their properties in their order of declaration are lexicographically ordered under `lore.core.less_than?`. You can override `lore.core.less_than?` with your own definition for any combination of (struct) types.



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

Comparisons of `Real` and `Int` implicitly convert the `Int` to `Real`.

To define **equality** for a non-basic, non-symbol type, you can specialize the function `lore.core.equal?`. Inequality is strictly defined as `!equal?(a, b)`.

```
module lore.core do
  func equal?(c1: Car, c2: Car): Boolean = ...
  func equal?(SportsCar, CheapCar): Boolean = false
  func equal?(CheapCar, SportsCar): Boolean = false  // Don't forget to be symmetric!
end
```

To define **order** for a non-basic, non-symbol type, specialize the function `lore.core.less_than?` and optionally `lore.core.less_than_equal?`. The latter is already defined as `less_than?(a, b) || equal?(a, b)` in Pyramid. Greater than, `a > b`, is strictly defined as `b < a`, and `b >= a` as `a <= b` .



### Function Calls

The syntax of a **function call** is simple:

```
target(a1, a2, ...)
```

**Call semantics** depend on the `target`:

- If the target is a **multi-function**, the compiler will simulate multiple dispatch to find the correct function definition. The semantics of such multi-function calls are defined in [multi-functions](multi-functions.md).

  We say *multi-function call*, because it only becomes a function call once a function has been chosen according to the dispatch semantics.

- If the target is a **function value**, the function will be called directly at run time. Anonymous functions and constructors will be called directly, but if the function value refers to a multi-function, multiple dispatch will of course still be performed at run time.

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
      str => String.length(str) < 5,
    ),
    str => '$str, world!',
  ),
  ' ',
)
```



### Multi-Function Values

A **multi-function value** is a *function value* created from a multi-function referenced by name but not called, e.g. `to_string` in `map(list, to_string)`. Compile-time dispatch is simulated with the expected argument types, which decides the output type of the function value. For example, if the argument types are `(A)` and dispatch finds a function `foo(a: A): B`, the resulting type of the multi-function value would be `A => B`. At run time, calling a multi-function value performs multiple dispatch normally, even though the multi-function is "hidden" behind a function value interface.

Multi-function values allow passing multi-functions as regular functions without wrapping them in an anonymous function. Their use is central to concise functional programming in Lore.

###### Example

```
[1, 2, 3] |> map(to_string)
```

`to_string` is a multi-function that is passed as a function value to the multi-function call of `map`. Dispatch of `to_string` is simulated with the input type `(Int)`, resulting in an output type `String`. The function value's type thus becomes `Int => String`. 

If a type context cannot be inferred, a variable declaration or a type ascription can be used to inform the multi-function value's type:

```
let f: Int => String = to_string

to_string :: Int => String
```



### Fixing Functions at Compile-Time

Instead of deciding dispatch at run-time, you can **fix a function at compile-time:**

```
f.fixed[T1, T2, ...]
```

The expression evaluates to a **function value** which may be subsequently invoked or passed around. See the section about fixed functions in [multi-functions](multi-functions.md) for a more in-depth explanation.



### Intrinsic Function Calls

Many functions in the Lore standard library, especially the most fundamental ones, defer their implementation to **intrinsics**, which are functions built into the Lore VM.

The syntax of an intrinsic function call is as follows:

```
intrinsic[ResultType]('f', a1, a2, ...)
```

The first argument is the name of the intrinsic, which must be a string constant. These are statically defined by the VM and cannot be added or removed by a Lore user. The compiler checks the existence and the arity of the intrinsic, but trusts the programmer about the argument types. 

Unless you're absolutely sure what you're doing, avoid using intrinsics. There will usually be a multi-function available from Pyramid that offers the intrinsic's functionality.



### Conditional Expressions

Lore supports a variety of **conditional expressions**.

##### If-Else

If the condition is true, the **`if` expression** evaluates to `tle1`, otherwise to `tle2`. Omitting the `else` branch leads to an implicit result type of `Unit`.

```
if condition then tle1 else tle2

if condition
  tles1
end

if condition
  tles1
else
  tles2
end
```

Note that either top-level expression (TLE) (or even `condition`) may be a block. The else part is, of course, optional. The so-called dangling else is always parsed as belonging to the `if` closest to it.

The if-expressions without a `then` require the top-level expression(s) to be placed on the next line. This also implicitly opens a block, which must be closed with an `end` or an `else`. An `else` with an implicit block must also be closed with an `end`. 

To avoid ambiguities, the `else` part of an `if` with a `then` must follow on the same line as the closing of `tle1`, which will usually be on the same line as the `then`. This restriction will be relaxed once we implement indentation-guided parsing. An example of a situation which needs to be disambiguated is:

```
if condition1
  if condition2 then tle1
else
  tle2
end
```

Does the `else` belong to the outer or inner `if`? With the restriction, it belongs to the outer. Without, there would be an ambiguity which would be parsed, due to the dangling else rule, as belonging to the inner `if`, contradicting the user's indentation.

##### Cond

The `cond` expression is more suitable than an `if` expression when many cases are involved. A `cond` case consists of a condition and a body, which is a single top-level expression. A `cond` expression evaluates to the body of the first case whose condition is true. 

The `true => ...` case simulates an `else` branch and is called the *total* case. A total case is optional, but it must always be the last case. If omitted, the "else" branch of the `cond` evaluates to `()` (unit).

```
cond
  condition1 => tle1
  condition2 => tle2
  ...
  true => tlen
end
```

A newline must always follow `cond` and it must always be closed with an `end`. Each case must be placed on its own line. If a condition should be a conditional expression, a loop, or an anonymous function, the condition must be enclosed in parentheses: `(if a then b else c) => 'fabulous!'`.



### Loops

**Loops** represent repeating control flow. For now, we support `while` loops and `for` comprehensions.

##### While Loop

A **while loop** repeats some piece of code as long as a given boolean expression is true:

```
while condition yield tle

while condition
  tles
end
```

We have decided to provide **no support for do-while loops**, because we feel that these kinds of loops are very rarely used, but add noise to the language in the form of an additional keyword being reserved (such as `repeat`). You should instead work with a function and a while or recursion.

##### For Comprehension

A **for comprehension** iterates over a list or map:

```
for e1 <- col1, e2 <- col2, ... yield tle

for e1 <- col1, e2 <- col2
  tles
end
```

In the syntax above, `col2` is fully iterated for each `e1` and so on, so supplying multiple extractors effectively turns the `for` comprehension into **nested iteration**.

For now, the only collections allowed are lists and maps, but we want to extend `for` comprehensions to arbitrary iterables.

##### Loops as Expressions

Loops are expressions, too. Similar to `if` expressions and blocks, the loop body expression determines the result of the loop. However, these evaluations are **aggregated into a list**.

Take the following example:

```
let names = for animal <- animals yield animal.name
```

The for comprehension **aggregates the names** of all animals in a list of type `[String]`. The compiler optimizes away the creation of the resulting list if it isn't used.



### Type Ascriptions

The **type ascription operator** `::` informs the compiler about the intended type of an expression. This can be used in situations where the compiler cannot infer a type, or doesn't infer the desired type.

The type ascription operator *cannot* cast a value's type to an incompatible type. For example, type ascription cannot be used to cast an `Animal` value to the `Fox` type. Such narrowing of types is currently not supported by Lore, but will become available in a future version.

###### Example

```
let mut option = Option.some(Fox() :: Animal)  // option: Option[Animal]
```



### Operator Precedence

The **operator precedence** is defined as follows, from lowest to highest precedence:

```
||
&&
== !=
< <= > >=
|>
:+
+ -
* /
::
! - (unary)
atoms (including function application)
```

Note that we don't view assignments as operators. **Complex expressions** such as conditionals or blocks cannot stand as an operand; you will have to enclose them in parentheses to use them with addition, for example:

```
5 + (do 10 end) + (if a == b then 5 else 15)
```
