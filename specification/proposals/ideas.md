# Ideas

This is a partly organized list of mostly loose ideas which may find their way into the language at some point. Especially important ideas are prefixed with a (!).



### Syntax

- (!) Seriously considering using indentation-based blocks. Multi-line lambdas are tricky then, but we can always allow optional do/end block syntax.
- Implement a pipe operator `t |*> f` that adds all elements of a tuple `t` to the arguments of a function call `f`. 



### Types

- (!) Add a native (hash) set type with the syntax `#[A]`.
- (!) Rename "sum types" to "union types" for better name duality with "intersection types"?
- (!) Lists are really easy to work with, but do we want users to write functions against lists or rather a sort of enumeration interface? Lists might be way too specific in some use-cases, especially when users want to implement their own collections and run into a wall of functions that only accept lists.
  - Are transducers applicable here?
- (!) How can we add an implementation of an "interface" `A` to a struct/trait `T` without access to `T`? Haskell's type classes or Elixir's protocols make this possible. How can we accomplish this with Lore?
  - Of course, we can add the required multi-functions, but the point is that we want `T` to be able to be typed as an `A`. Dynamic specialization is not the answer here, because this is already known at compile-time.
- Re-add default names for tuple elements.
  - When an element is an intersection type, simply use the first nameable part to build the name. The first part is likely the "main" part.



### Traits/Structs

- (!) Add `struct instance` declarations which are basically singletons/objects. Alternative keywords: `instance`, `object`, `singleton`.



### Multi-Functions

- Function invocation without parentheses?
- Predicate dispatch!



### Modules

- Type parameters for modules?
- Implicit parameters for modules?



### Expressions

- (!) Anonymous functions with unnamed parameters such as `map(list, _.name)` could additionally be defined with multiple parameter positions. `($1, $2)` would create a function `(a, b) => (a, b)`. The difficulty is: where is the boundary of the anonymous function? Why wouldn't `($1, $2)` be `(x => x, (y, z) => z)`?
- Think about removing `return`, as it is annoying to implement and reduces the straight-forwardness of the language. On the other hand, `return` helps avoid some nesting.
- Build a list from optionals: `[if (a) x, if (b) y, if (c) z] |> flatten`. The trick here is that an `if` expression without a corresponding else should return an option.
  - This would also be applicable to `cond` and `case`.
- Delayed computations: https://www.unisonweb.org/docs/language-reference#delayed-computations
- Watch expressions for rapid development: https://www.unisonweb.org/docs/quickstart/
- Provide an easy way to update immutable structs and shapes. For example, Scala's case class `copy` function.



### Documentation

- (!) Documentation as a first-class feature of the language. See also: Unison, Elixir.
- Documentation that contains *compilable source code examples* so that Lore code inside documentation is also checked.



### Implicit Parameters with Annotations

```
struct Counter
  mut value: Int
end

<given counter: Counter>
func more(): Unit = counter.value += 1
// or
@given counter: Counter
func more(): Unit = counter.value += 1

// The variable name can be omitted if the implicit parameter is simply passed on.
@given Counter
func even_more(): Unit = do
  more()
  more()
end
```

Inspiration: Java annotations, Nim pragmas.



### Algebraic Effects

- Look at algebraic effects for exception handling: Unison, eff-lang.org, Scala Effekt.
- Algebraic effects can also be used to model asynchrony, IO monads, compilation error reporting, and much more.



### Performance

- The most important goal to optimize multiple dispatch is this: "Minimize the number of redundant subtyping checks."
  - We can cache checks if necessary.
- To optimize multiple dispatch further, we have to avoid as many checks as possible. Perhaps the idea of **mutually exclusive types** can be of help here. For example, a 2-tuple and a 3-tuple are always exclusive. There is no way that a value could inhabit both types. The same is true for anonymous functions with different arities. There could be "marker" types which segregate the dispatch search space into small subsets. For example, if we have a function `f(a, b, c)` and `b` and `c` vary wildly, but `a` can be segregated into three different categories, we would *first* check `a`, thereby rule out a huge number of type checks, and then check `b` and `c`.
  - In general, we want to first check the type that rules out the most branches. This can probably be localized to "layers", meaning that we optimize this for each layer of sibling functions with the same specificity. The most crucial is the root layer. However, multi-layer checks may also be necessary to catch most redundant subtyping checks.
    - If we can check a single parameter type first, we can also build a split dispatch cache that works in two or more layers.
  - There are probably some restrictions to this when type variables are involved. The great thing about viewing parameters and arguments as tuples is that we can check their fit for each element individually, and out of order. But this only works when these individual types are independent of each other. Type variables complicate this, because they create a "global" interdependence between parameters.
    - That is not to say that we can't check the fit of parameters out of order even with type variables, at least partially. Maybe the magic word here is "rule out". If we have a function whose second parameter is a tuple `(A, Animal)` with `A` being a type variable, we can still rule out this branch if the argument is not a tuple, or if the argument is a tuple whose second element is incompatible with `Animal`.
  - We have to be careful with this, however, as hierarchies aren't always readily apparent. There is no question that a list and a tuple are mutually exclusive, but traits and structs get vastly more complicated with multiple inheritance and, possibly still planned, dynamic specialization.
  - In the spirit of this optimization, we should also add an optimization for how some trait/struct-heavy multi-function hierarchies are handled. There are certainly single-dispatch-like multi-functions which could be compiled down to a lookup table. We just have to identify them.
- The ideal dispatch algorithm would not even delegate to functions like `isSubtype` unless absolutely necessary. If we want to check that a given type is a tuple that contains the elements `Animal` and `String`, we don't go `isSubtype(argumentTuple, tupleType(Animal, String))`, we go `argumentTuple.kind === Kind.Tuple && argumentTuple.elements.length === 2 && isSubtype(argumentTuple.elements[0], Animal) && argumentTuple.elements[1] === String`. That's inherently cheaper, because we have to process fewer function calls, fewer match statements, and much more. I doubt that V8 will inline all of this at run-time. The motto here is: "Do the same with less!"



### Platform

- WebAssembly could be a good long-term compilation target if we want to grow beyond Javascript.
  - It currently has no built-in GC support, but there is a proposal for it. However, building a simple GC would be a cool project and probably be doable in at most a few weeks.



### Macros

- Interesting macro use case: Data backed by a state machine with explicit transitions.

