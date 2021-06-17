# Flexible Totality Constraint

Consider the following piece of code:

```
trait A
trait Y

struct A1 extends A
struct A2 extends A, Y

function f(a: A): Int
function f(a: A1): Int = 2
function f(y: Y): Int = 1
```

Essentially, `f` needs to be defined for `A1` and `A2`. The former is covered by `f(a: A1)`, while `A2` should be covered by `f(y: Y)`, because `A2` extends the trait `Y`.

Our current definition of the **totality constraint** does not allow this constellation, however. The constraint expects that there is a function *more specific* than `f(a: A)` that implements `f` for `A2`. `f(y: Y)` provides the correct implementation but loses with the "more specific" requirement.

There is a **reason** why the totality constraint is this strict: It's simple. We don't have to consider any of the edge cases that would arise from allowing implementations of functions to be specified across the *whole* hierarchy of a multi-function rather than a well specified subsection of the graph. But it is also annoying. It seems right that the code above should work. The function *is* implemented for `A2`. It would work at run-time. And ultimately, the totality constraint should mirror the workings of run-time dispatch.

This **proposal** takes a first step in examining the implementation issues that might arise with such a change to the totality constraint. We survey all the probably necessary changes.

#### Change 1: Totality Constraint

We'd have to change the **totality constraint** itself, of course. This isn't trivial as it can lead to circular reasoning:

```
trait P
trait Q
struct X extends P, Q
function f(p: P): Int
function f(q: Q): Int
```

This program compiles because `f(p: P)` and `f(q: Q)` cover each other. So to implement a proper check, we would have to implement an algorithm that can detect these cycles, which is not trivial.

##### Algorithm Proposal

Here is what such a totality constraint checking algorithm could look like: 

```
check(mf) {
  for all abstract f in mf {
    for all ARDS t of in(f) {
      isDispatchedToConcreteFunction(mf, t)
    }
  }
}

// This function would be a candidate for memoization, so that
// complexity doesn't explode.
isDispatchedToConcreteFunction(mf, f, t) {
  // Simulate multipe dispatch for the input type to get the
  // function that's actually called at run-time.
  min = dispatch(mf, t)

  if min is ambiguous: error  // TODO: Or return false?
  if min is empty: error      // TODO: Or return false?

  f2 = min.head

  // As f2 can possibly be a function that isn't a subordinate of
  // our initial function f, we also have to check that the return
  // type matches.
  if not out(f2) <= out(f): return false
  
  if f2 is concrete {
    true
  } else {
    // Essentially we want to check that the input type t is covered by
    // a concrete function. If the function f2 is itself abstract, we
    // instead check that all ARDS of t are dispatched to concrete 
    // functions.
    for all ARDS t2 of t: isDispatchedToConcreteFunction(mf, f, t2)  
  }
}
```

#### Change 2: Ambiguity in Multiple Dispatch

In the example above, if we called `f` with a value of `a2`, we'd run into an **ambiguity**, because both `f(y: Y)` and `f(a: A)` are valid targets and neither is more specific than the other.

The supposed ambiguity between `f(a: A)` and `f(y: Y)` is artificial because `f(a: A)` technically doesn't exist at run-time; it is simply declared to provide typing hints and existence guarantees to the compiler.

We could just throw abstract functions out of the min-set at compile-time, but this seems to be too simple. The change also has many, many implications and might not be the right step.

There are other questions, too: What if `f(y: Y)` was also abstract? How would we choose between `f(a: A)` and `f(y: Y)`? And what if the return type of `f(y: Y)` was simply wrong? (See [this file](./wrong-return-type.lore) for an example.) 

#### Change 3: Run-time Dispatch

Right now, the **run-time multiple dispatch algorithm** assumes that when going into an abstract function like `f(a: A)`, we won't be able to find any other path like `f(y: Y)`. So an error is thrown by the runtime if the abstract function is tried but no subordinate functions are found. This works fine as long as the totality constraint is as strict as it is now. But if we relax the totality constraint, we will have to make a change to this behavior.

#### The Smoking Gun?

The **return type** example above might be the smoking gun that kills this proposal. If `f(y: Y)` provided an implementation for `f(a1: A1)` (in the [linked example](./wrong-return-type.lore)), we'd also have to check that `f(y: Y)` adheres to the return type of `f(a: A)`. So suddenly, we have to include return types in the totality constraint check.

If this is possible, the proposal can live on. But is it possible in the general case? Do we want to solve the general case or just provide utility to Lore programmers? And can we hold up our run-time type guarantees? So far, at least return types have been sound and they need to continue to do so.

It might also be that checking the return type is possible, but then we'd also have to **implement this check at run-time**. If that is the case, we can more or less forget this proposal, as it would not be in the spirit of multiple dispatch: the dispatch semantics shouldn't rely on the return type, only the input type.

#### Outlook

I hesitate to implement this now because of the difficulty of implementing the right checks and the danger that this is actually wrong and will break Lore programs in one way or another. So before implementing these changes, it would be prudent to do the following:
 
1. Expand the test base around multiple dispatch and abstract functions.
2. Work out many more edge cases (including cases where multiple parameters are abstract) that should work *now* and continue to work and ones that don't work now but should work *then*. Maybe we can infer the right implementation of the totality constraint from more examples of this kind.
3. Work out many examples around preferring concrete over abstract functions to solve ambiguities, or some other way to solve the `f(y: Y)` and `f(a: A)` ambiguity.

After these changes and further examination, we might consider the proposal.
