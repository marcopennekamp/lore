SITUATION 1:

The run-time performance is atrocious. Calling hello-name's testPerformance (excluding printlns) with an iteration count 
of 100,000 takes about 7300ms in Deno, using fully optimized ScalaJS and having run the program before (so that the JIT has 
time to optimize). This means that the evaluation of one test() takes about 0.073ms. The corresponding hand-coded JS 
program (which also doesn't print the result but assigns the string to a global variable) takes about 14ms. So 100,000 
iterations of the hand-coded JS function run as fast as 200 iterations of the Lore function. I don't want to reach 
native JS performance, of course, because it's OK if multiple dispatch is a little slower. But it should at least be 
in the ballpark. We'd be talking about 50-100ms for 100,000 iterations. 

GOAL: 100,000 iterations take 50-100ms on my local machine.

CHANGES 1:

- Remove useless sum/intersection type simplifications with construct from already simplified sum/intersection types.
  DONE. ~8% performance improvement.
- Remove partial functions from the subtyping algorithm, defining it more directly, which leads to a lot less overhead.
  DONE. 30-40% performance improvement.
  
  
  
SITUATION 2:

At this point, I went deeper into the analysis and found out that just the virtue of using ScalaJS added a lot of 
useless overhead on top. For example, creating objects with complex class hierarchies, converting arrays to lists,
type checks at the boundary between transpiled Lore JS and ScalaJS internals, and so on. Objects are far heavier
than they need to be, lists are far heavier, in some places our usage of sets slows us down, etc. And all this to
save a "few" lines of code.

CHANGES 2:

- I completely rewrote the runtime in Typescript, trying my best to write fast code. Types and values are now simple
  Javascript objects, which leads to a lot less overhead. In general, we can report the following performance gains:
  - 100,000 iterations take about 400ms. This is still not close to our performance target, but much much better than
    the previous 4000ms (after optimizations had been applied). This represents a gain of 10x.
    - One call of test() takes 4µs! Pretty good.
  - The native JS function still takes 14ms, so we have the following comparison:
    - One call of the native test() takes 0.14µs, compared to Lore's 4µs.
    - 100,000 calls of the native test() take as much time as 3,500 calls of the Lore function.



SITUATION 3:

We're measuring the performance to see where we can still optimize.

CHANGES 3:

- A simple optimization: Only substitute in fits if the type is actually polymorphic. This was an oversight which led
  to a lot of useless iteration and such.
  - Improvement: 400ms -> 300ms at 100,000 iterations
- Use an array-based map instead of Map for type allocations and assignments. Since hashed Maps come with substantial
  overheads, the array is faster for small sizes. (We can also consider doing this on the compiler side if performance
  ever becomes an issue there.) 
  - Improvement: 300ms -> 250ms at 100,000 iterations 
- In the transpiled multi-function, replace the function Set() with a simple array and implement the uniqueness check
  manually. The array should usually only be a few elements big at most, so that way might be quicker than some Set
  magic.
  - Improvement: Almost no gains in Deno/V8. Firefox shows a bit more improvement. Since this was discovered with the 
    Firefox profiler, perhaps V8 had already applied some form of optimization. In any case, this isn't a bad change,
    since only very few chosen functions will ever be part of the tiny set / array. (In fact, )
- Get rid of the args.map in each multi-function header (where the input type is constructed). A plain loop would seem
  to be faster and more direct anyway.
  - Not faster, but more direct (fewer JS API calls), so it will stay in.
- We can move the input types out of the multi-function definition above into the global scope so that the 
  value is cached instead of recreated every function call.
  - No significant impact.
- For each multi-function, keep a cache that remembers for which input type which function was called.
  - Improvement: 250ms -> 110ms
  - After implementing a custom hash map and custom hashing functions (murmurhash3), we could implement dispatch 
    caching quite easily. I suspect most performance gains come from the caching in append, since computing type 
    allocations is still quite costly, and possibly caching of hello, since its dispatch structure is not quite 
    trivial. It is possible that there is a performance loss for simpler dispatch structures, especially those 
    without any parameters at all, as computing the input type's hash is not cheap either.
- Generate fitsMonomorphic or fitsPolymorphic calls based on whether the right-hand type is polymorphic at compile-time. 
  This is possible because the right-hand type is known by the compiler already.
  - No performance impact when dispatch caching is enabled.
- Disable the dispatch cache for trivial dispatch contexts.
  - Improvement: At most 5ms (110ms -> 105ms). This is quite hard to measure so we should assume no big impact at all.
  - I'm expecting this improvement to become larger as we implement the next optimization in the list. 
- We don't need to keep a list of chosen functions in the multiple dispatch implementation. We can keep a single running
  variable. If it is already filled and we want to choose a second function, we throw the ambiguity error. If it is 
  unassigned to at the end, we throw the empty-fit error. In other cases we can cache and call the function.
  - Improvement: Minuscule. At most 2-3ms.
- Declare the size of the argument types array right away and assign argument types via indexing instead of push.
  - Improvement: About 5-10ms. Not bad!
- Bypass the argument type construction for "single unit multi-functions" entirely and simply assign unit
  to the argument type.  
  - Improvement: Another 5-10ms. Now running in about 90-95ms on my laptop.



FUTURE:
- Pull the list type that is created in test$0 into the constant scope. In general, try to factor all type constants
  that are created anywhere inside a function out of that function, because V8 doesn't do that.
  - In a manual test, moving the list type creation in test$0 to the global scope improved performance from about 110ms 
    to about 90ms.
- If there is only a single function, we can vastly simplify the dispatch function, as we only need a single fits test.
- If the input type is a tuple with a single element, we can technically bypass creating the product type, but would
  need to mirror that on the right-hand side.
- If all function arities are the same, get rid of the loop for the input type and "hard-code" the construction of the
  input type array.
- isSubtype and fits both test for equality of t1 and t2. We could call an internal isSubtype from fits that doesn't do
  this comparison (again) and thus save a single comparison.
- Turn functions calls which don't rely on multiple dispatch into direct calls. This is especially useful for generic 
  functions, because (1) a function with type parameters is less likely to have multiple implementations and (2) checking 
  fit for generic functions is costly.
- BIG GAINS: Dispatch calls to subtrees of the multi-function, because with static types we can easily rule
  out which functions we need to check against in the first place. If we have a type C at runtime, we don't need to
  check its superclasses A or B. This will save a huge amount of "fits" calls, especially with large multi-functions.
  - Implementation idea: Since we can't access goto in Javascript (unless we are in a while-loop context), we could 
    perhaps introduce "bypass_fitsX" variables, which could be used to skip fits calls and then walk into the part
    of the tree where we actually need to decide dispatch. 
  - Considering this, it is paramount that we have as few fits calls as possible. I could easily see deep and, 
    especially, BROAD class hierarchies lead to performance problems. If we implement functions for 100 direct 
    subclasses (this scenario sounds weird but isn't quite so daunting if we consider splitting the function over
    many, many files) and the runtime has to check the fit 100 times, the program will die a very slow death. However, 
    there are again optimizations to be found here, though they won't be trivial (and might not be worth it with 
    caching).
    - The thing with caching is that I assume IF a function needs to be called many times, it will already have been
      called with the same types before, so calling it will be fast. 
      - We can also someday implement multi-level caching. Basically, if type 1 is a subtype of this, go to A, if type 
        1 is a subtype of that, go to B. A little bit like database indices.
        - The index approach is a pretty cool idea. Maybe an index structure would be optimal for a subclass of types,
          most likely simple dispatch structures that don't have complex types. For example, if we only decide dispatch
          based on simple classes (without even intersection types), we can easily build such an index. 
        - We could build such an index at compile-time and use it if the run-time input types are simple enough to 
          agree.
- Entirely split multi-functions based on arity. This would also allow us to unroll the loop when constructing the
  input type tuple.
- Ensure that all unit types are the same? 
