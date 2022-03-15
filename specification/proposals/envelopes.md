# Envelopes

An **envelope types** wraps a value of another type. An envelope is declared and constructed as follows:

```
envelope E(V)

let e = E(v)
```

The envelope's **constructor** is a function value, similar to struct call-syntax constructors. The type of constructor `E` above is `V => E[V]`.

The **purpose** of an envelope is to give semantic meaning to a more general data type. For example, a phone number can simply be saved as a string, but it would be advantageous to have a `PhoneNumber` type instead.

Instead of **accessing the underlying value** directly, the envelope type is used exactly like the underlying value. In any type contexts where the underlying type (or a supertype of it, excluding supertypes of the envelope itself) is expected, the compiler implicitly accesses the underlying value of the envelope. A member access on the envelope acts as a member access on the underlying value.

- **TODO:** We'll have to evaluate whether this is feasible, especially in conjunction with multi-function type inference.

- We have to be careful if we wanted to use subtyping such that `E < V`. Suppose we have the following code:

  ```
  trait A
  trait B extends A
  envelope X(A)
  
  func foo(A): String = 'hello'
  func foo(B): String = 'world'
  ```

  What if we invoke `foo(X(b))` with `b` being a subtype of `B`? If an envelope merely subtypes its value type, multiple dispatch would choose to invoke `foo(A)`. So the subtyping relationship would have to be extended to `E < B` iff `E` contains a value that subtypes `B`. This would lead to `foo(B)` becoming the call target.

- Ultimately, we can't solve the problem with subtyping, because at run-time, accessing a value `v` is different than accessing `e.v`. We would have to wrap any kind of value access (passing to an intrinsic function, member access, printing, equality, etc.) in a function that checks whether the current value is the expected value or still wrapped in an envelope.

Envelope types have two important **advantages:**

- They provide a convenient way to **document** your code without writing comments. For example, take a function `score: String => Real`. With envelope types, this could be `score: Player.Name => Player.Score `. Much clearer, right?
- They improve **correctness resilience** through type checking. Since you can't pass the underlying value without constructing an envelope, you will not accidentally pass any value that doesn't (theoretically) belong there. For example, take the `score` function from above. You will have a hard time passing your grandma's name (who certainly doesn't play games!) or even another envelope type such as `Location.Name`.

###### Abstractness

An envelope type is abstract if its **underlying type is abstract**.

###### Examples

```
envelope PhoneNumber(String)
envelope Players([Player])
envelope Height(Real)
```

