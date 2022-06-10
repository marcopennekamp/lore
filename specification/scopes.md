# Scopes

Lore supports **lexical scoping** for both terms (variables, multi-functions, etc.) and types, collectively called bindings. This document outlines the various kinds of scopes and how they interact.

Also check **name resolution** in [modules.md](modules.md).



### Type Scopes

Currently, there are four kinds of **type scopes** in Lore:

- The **root type scope** contains all named types declared in the root module.
- A **module type scope** contains named types declared in a module.
- A **function type scope** contains the type variables declared in a function's `where` clause. 
- A **trait/struct type scope** contains the type parameters of a trait or struct.

These scopes **shadow** each other as they nest. For example, if you declare a type variable `Apple` in a function, any type named `Apple` in the registry type scope won't be accessible from that function.

If a type `A` and its **companion module** share a name, a type access `A` resolves to the type, but any types declared in the module will still be accessible: `A.Foo`.



### Term Scopes

Terms are global variables, multi-functions, struct constructors/objects, modules, and local variables. Currently, there are four kinds of **term scopes** in Lore:

- The **root term scope** contains all terms declared in the root module.
- A **module term scope** contains terms declared in a module, as well as nested modules.
- A **function term scope** contains the parameters declared in a function's parameter list.
- A **block term scope** contains all local variables declared in a block. Block scopes nest indefinitely.

These scopes **shadow** each other as they nest. 

If a multi-function or global variable share a name with a struct, the multi-function or global variable is preferred over the struct's constructor/object. Struct constructors/objects and their companion modules are accessible simultaneously.
