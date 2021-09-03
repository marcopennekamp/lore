# Scopes

Lore supports **lexical scoping** for both bindings (variables, multi-functions, struct constructors, etc.) and types. This document outlines the various kinds of scopes and how they interact.



### Type Scopes

Currently, there are four kinds of **type scopes** in Lore:

- The **registry type scope** contains all global named types, including all traits, structs, and type aliases. It also contains top-level modules.
- A **module type scope** contains named types declared in a specific module, as well as nested modules.
- A **function type scope** contains the type variables declared in a function's `@where` annotation. 
- A **trait/struct type scope** contains the type parameters of a trait or struct.

These scopes **shadow** each other, with the last mentioned scope having the highest priority. For example, if you declare a type variable `Apple` in a function, any type named `Apple` in the registry type scope won't be accessible from that function.

If a type `A` and its **companion module** share a name, a type access `A` resolves to the type, but any types declared in the module will still be accessible: `A.Foo`.



### Binding Scopes

Bindings are local and global variables, multi-function, and struct constructors. Currently, there are four kinds of **binding scopes** in Lore:

- The **registry binding scope** contains all global multi-functions, struct constructors, and modules. If a multi-function and struct constructor share the same name, the multi-function is preferred. Module names have the least priority.
- A **module binding scope** contains multi-functions and struct constructors declared in the module, as well as nested modules. If a multi-function and struct constructor share the same name, the multi-function is preferred.
- The **function binding scope** contains the parameters declared in the function's parameter list.
- **Block binding scopes** contain all local variables declared in the given block. Block binding scopes nest indefinitely.

These scopes **shadow** each other, with the block binding scope being the most preferred scope.



