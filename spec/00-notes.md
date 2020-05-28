# Notes

#### Some Opinions

- **Lore does not have a null.** You *have* to use `Option[A]` instead (or maybe `A?`), *especially* when you're interfacing with foreign language code. That means you have to declare any value you receive from a foreign language as an `Option` if you have any reasonable expectation that the value will be null (in which case the null value will be converted to None). Receiving a null value from foreign code *without* declaring the value as an Option results in an immediate *runtime exception*.
  - **Possible Exception:** Local variables or maybe even record properties may be uninitialised, but they won't be usable (meaning runtime error) before they are initialised.
  - **TODO:** `Option` support should be as native as possible.
- **Equality should be explicit.** I don't like providing DEFAULT equality, like JVM languages do. A type should be explicitly comparable before it can be compared; that will help with issues like forgetting to implement equals/hashcode for a type that is used as a map key.



#### Problems

- The term **component type** is ambiguous. It usually refers to a proper kind of type `+C`, but may also be used for "the type of a component of an entity." Maybe we can find another name for `+C`, one that better reflects the nature that a value of `+C` is an *entity* value, not a component value.
- Maybe we should introduce a restriction that an intersection type **can only ever contain one class type**. This is simply for practical reasons, because it is currently impossible to create a value which would be of two different classes, except for a construction such as `A & B` for `A < B`. But `A` is trivially a subtype of `B`, so `A & B` is a nonsensical construction. In summary, it makes no sense to have more than one class in a single intersection types (at least as far as I can see), so we should disallow it so as not to confuse the programmer. We can always ease the restriction if such an intersection suddenly makes sense.
  - One feature that would make such intersections useful would be **multiple inheritance**, because then we can have a constellation of multiple classes inheriting from two independent classes `A` and `B` each, and defining functions over such intersections `A & B`.
  - We should also throw a warning when an **entity and component** intersect that aren't even compatible. For example, given an entity `E` that doesn't have any relation to a component of type `C`, we should throw a warning for a type such as `E & +C`. This is especially problematic because the programmer could code against that interface, only to find out that `E` doesn't even have such a component.
  - **Lists and maps** should be exclusive as well, of course.



#### Ideas for later

- Rename multi-functions to **multi-methods** and functions to methods to signal that they implement dispatch in contrast to "simple" function that are treated as values?
- Multi-functions should **only dispatch over the first parameter list**. (Once we support multiple parameter lists. This is actually quite a nice idea to separate dispatched and non-dispatched values.)
  - **Alternatively:** Require the developer to specifically declare parameters as either "dispatched" or "static", depending on which is the sensible default. This could however mess with extendability, as a library user might wish to dispatch on a static parameter.
  - The **dot notation** for multi-functions should require the "dotted" value to correspond to the first parameter list. In particular, if we dispatch over two parameters `a` and `b`, dot notation needs to be invoked like `(a, b).collide()`.
- The idea of **functional reactive programming** is an interesting technique that might be appropriate for game development. I'd like to delve into this concept and maybe promote it for use in Lore. While it is unlikely that FRP will be a first-class language feature, we should take special care with the language design to keep it FRP-friendly. If we want to support FRP, we should also ensure that FRP frameworks can be used with Lore and provide examples of using some frameworks. Finally, we should explore how FRP can be best used with the language features that Lore provides.
  - Some reading that may serve as a conceptual starting point:
    - [bacon.js](https://github.com/baconjs/bacon.js)
    - [RxJS](http://reactivex.io/rxjs/manual/overview.html)
    - [Elm Language](https://guide.elm-lang.org/install.html)
    - [Helm (Game Engine)](https://github.com/z0w0/helm)
    - [Haskell Wiki FRP Libraries](https://wiki.haskell.org/Functional_Reactive_Programming#Libraries)