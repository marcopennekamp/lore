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



FUTURE:

- Measure the performance of the new Typescript runtime with the Firefox profiler.
- We can move the input types out of the multi-function definition above into the global scope so that the 
  value is cached instead of recreated every function call.
  - I suspect this is a major contributor to slowdown currently, because the subtyping algorithm should be quite
    fast 
- For each multi-function, keep a cache that remembers for which input type which function was called.
- Turn functions calls which don't rely on multiple dispatch into direct calls. This is  especially useful for generic 
  functions, because (1) a function with type parameters is less likely to have multiple implementations and (2) checking 
  fit for generic functions is costly.
- In the transpiled multi-function, replace the function Set() with a simple array and implement the uniqueness check
  manually. The array should usually only be a few elements big at most, so that way might be quicker than some Set
  magic.  
- Don't use Maps in fit.ts. Rather use arrays and manually compare 
