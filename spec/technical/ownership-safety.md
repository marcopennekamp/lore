# Ownership Safety

**Specializing ownership** is not fully type-safe at compile-time. For example, consider this piece of code:

```
trait A
struct A1 implements A
struct A2 implements A

trait B owned by +A
struct B1 implements B owned by +A1

struct E { component A, component B }

function create(): E = {
  let a: A = A2()
  let b: B = B1()
  E(a, b)
}
```

The compiler approves this layout, but because `B1` narrows the ownership restriction, `B1` is *not* owned by an entity `+A1`. It is clear that this code cannot go ahead as is. The question is when we throw the error. We have two ways to go about this:

- We could make owned-by declarations **invariant**. That is, when an owned-by type has been declared, it has to be replicated across the whole inheritance hierarchy. I don't see another (straight-forward) way to solve this while still keeping component subtyping intact. We would either have to get rid of the ability to use subclasses as components (e.g. use `B1` in place of `B`), or we would have to get rid of the ability to subtype ownership restrictions.

- What do we do if we can't check it at compile-time? **We check it at run-time!** That is, instead of giving compile-time guarantees that ownership restrictions hold, we check ownership each time an entity is instantiated. At compile-time, we only check ownership for the declared component types. This means that ownership won't be arbitrary. **We will still check what we can at compile-time.** 

  **At run-time**, we get down into the actual component types, that is, potentially their subtypes. Components being immutable, we only have to check this when an entity is instantiated, so there are, I feel, few enough points of failure.

In the end we have to choose between safety and flexibility. I believe Lore should be flexible rather than safe; multi-functions aren't perfectly safe, either. For example, we always run the risk to arrive at ambiguous calls at run-time, which might never have been caught at compile-time. Instantiating entities is in the same rough category of run-time errors. Hence, we have chosen the second option, to **allow subtyping owned-by types**, with a **hybrid checking model**.

Besides, I believe this will follow the **actual usage patterns** of the language. A constellation such as the example above could be very common. One might desire to have different kinds of `Position` components tied to different kinds of `Speed` components. If we don't allow subtyping owned-by types, that will not stop Lore users who *want* to subtype owned-by types. And so they might resort to a less idiomatic solution such as type-casting, or throwing runtime exceptions (once we actually add exceptions) from one branch of multiple dispatch, that is, the branch that is not supposed to exist.