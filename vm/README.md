This VM is actually meant to run Lore in the near future. We'll see how that pans out!

Advantages over a Javascript runtime:

- The core runtime code such as type equality, subtyping, type construction, and collections is much faster in Nim than Javascript. While the performance of V8 is impressive, Nim manages to e.g. squeeze out 2x to 4x more performance for type equality. This is mainly due to the ability to turn off runtime checks (such as bounds checks) for performance-critical code, which Javascript does not provide.
  - The main point here is that the core run-time structures of Lore can be implemented natively using Nim, as opposed to with one layer of indirection with Javascript. In Nim, we just define objects for types, values, etc. In Javascript, a Lore type is a JS object, which itself has metadata associated with it. This of course translates to better performance and a (hopefully) smaller memory footprint, but also provides a cleanness which is to me preferable.
- The compiler will not have to optimize multiple dispatch at all, because that burden can be shifted to the VM. This makes the compiler considerably leaner. 
- Nim provides native threading, while Javascript is single-threaded. Implementing parallelism for Lore in Javascript would be hard or impossible. 
- We can implement a native C FFI between the Lore VM and other C-compatible code. This will make it relatively easy to integrate Lore into games and with other compiled languages.
- Nim's different options for garbage collection give us a good opportunity to fine-tune the VM in that respect. 

Some caveats:

- The actual Lore code and maybe multiple dispatch will likely *not* be faster than with the JS runtime. This is simply due to the fact that V8 JITs the JS code, while the Lore VM will be interpreted until we can implement a JIT compiler as well. How this impacts the average Lore program is unclear. The programs might be faster due to the vastly improved speed of core functionality or select optimized operations such as field access (due to statically typed structs), or they might be slower due to the vastly worse speed of interpreting bytecode. 
- The VM will not be as portable as Javascript code, especially if we want to run Lore in the browser.
