# Runtime Types

Since Lore decides multiple dispatch based on actual types at run-time, we have to decide *when* and *how* certain types get assigned. Due to reasons of semantics, practicality, and performance, we cannot type all values to arbitrary depth at run-time.

In general, we have the following rule: **A value's type must be immutable once the value has been constructed. It must not change during subsequent mutations of the value.**

Hence, we define, for *values*, the types they have at run-time:

- Any basic values, meaning numbers, booleans, and strings, get their respective actual types assigned at run-time.
- A tuple gets its actual product type assigned during construction based on the actual types of its elements' values at run-time.
- A list's type is decided at compile-time? <-- This is looking increasingly dire.
  - TODO: How does concatenation work?
    - The function would have the signature `concat(as: [A], bs: [B]): LUB(A, B) where A, B`, with LUB as the type operator that would result in a least upper bound of types A and B. We'd somehow have to mirror that type computation to the runtime...  
  - We might technically be able to type IMMUTABLE linked lists by deciding LUBs based on an added element and the current list (if added). The type of the list without the added element would be preserved as the tail of the new list. However, I don't think this is generally possible for, say, mutable arrays. How would we adjust the type of the list if the element at a given index is replaced with a value of another type. This might work in one direction, when making the type of the list more general, but might not be possible when we would have to specialize the type because a more general element got removed or replaced. We'd need to support both directions to make it useful and consistent.  
    - Maybe we should implement default lists as immutable Scala-like Vectors and see how we can actually support at least type widening at run-time. This would make it easier to type empty lists as `[Nothing]` without having to resort to magic 
- Because maps are mutable, their type is decided when they are constructed. This also means that we will have to pass key and value type parameters to maps unless the type can be immediately inferred from the given entries. Since maps are invariant, a careless coder won't be able to accidentally pass a `Nothing -> Nothing` map to some other function expecting a, say, `String -> Int` map.
  - I'd propose the syntax `%{ } :: String -> Int` for typing maps explicitly.
  - Given this requirement, maybe we should introduce the following rule: Given a map LITERAL (or, possibly, list/array literal), the map's actual type at runtime is determined by the surrounding, type-giving expression. In practice, this would mean that assignments, variable declarations, function calls, etc. have the ability to implicitly type a map literal:
    ```
    const map: String -> Int = %{ } // valid
    class C { mut map: (String | Int) -> Int }
    const c = C(%{ }) // valid
    c.map = %{ } // valid
    c.map = %{ 'hello' -> 20 } // valid, even though the literal has a different type
    c.map = %{ 'hello' -> 'world' } // invalid, because the literal has an incompatible type
    
    action foo(map: (String | Int) -> Int) {  }
    foo(%{ }) // valid
    foo(%{ 10 -> 20 }) // valid
    ```
- A class
  - When an object is constructed, the type is decided right away based on the run-time TYPE CONTEXT. For example, when we call a multi-function with type parameters, these parameters are assigned types based on some type allocation. So these types would then be used in the actual type of a given created class instance.
    - Simple example:
      ```
      class C[A] { x: A }
      function create(x: A): C[A] where A = C[A](x)
      create('hello') --> C[String] at run-time
      create(15)      --> C[Int] at run-time
      ```
  -  We should introduce an LUB operator so that types can be combined. For example, let's say we have a concat function for a class `Collection[A]`. We want to express a function `concat(a: Collection[A], b: Collection[B]): Collection[LUB(A, B)] where A, B = Collection[LUB(A, B)](... some concat magic)`. The class type would be instantiated at RUN-TIME with the LUB of the actual types A and B. This would be necessary to correctly type classes in many cases.
- An entity 
