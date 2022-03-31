# Ideas

This is a partly organized list of mostly loose ideas which may find their way into the language at some point.



### Syntax

Seriously considering using indentation-based blocks. Multi-line lambdas are tricky then, but we can always allow optional do/end block syntax.
- Implement a pipe operator `t |*> f` that adds all elements of a tuple `t` to the arguments of a function call `f`.



### Multi-Functions

- Predicate dispatch!



### Expressions

- Anonymous functions with unnamed parameters such as `map(list, _.name)` could additionally be defined with multiple parameter positions. `($1, $2)` would create a function `(a, b) => (a, b)`. The difficulty is: where is the boundary of the anonymous function? Why wouldn't `($1, $2)` be `(x => x, (y, z) => z)`?
- Build a list from optionals: `[if (a) x, if (b) y, if (c) z] |> flatten`. The trick here is that an `if` expression without a corresponding else should return an option.
  - This would also be applicable to `cond` and `case`.
  - However, this would mean that the compiler needs to know about Options.
- Delayed computations: https://www.unisonweb.org/docs/language-reference#delayed-computations
- Watch expressions for rapid development: https://www.unisonweb.org/docs/quickstart/



### Documentation

- Documentation as a first-class feature of the language. See also: Unison, Elixir.
- Documentation that contains *compilable source code examples* so that Lore code inside documentation is also checked.



### Implicit Parameters with Annotations

```
struct Counter
  mut value: Int
end

@given counter: Counter
act more() do
  counter.value += 1
end

// The variable name can be omitted if the implicit parameter is simply passed on.
@given Counter
act even_more() do
  more()
  more()
end
```



### Algebraic Effects

- Look at algebraic effects for exception handling: Unison, eff-lang.org, Scala Effekt.
- Algebraic effects can also be used to model asynchronicity, IO monads, compilation error reporting, and much more.



### Macros

- Interesting macro use case: Data backed by a state machine with explicit transitions.
