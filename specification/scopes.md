# Scopes

Lore supports **lexical scoping** for both bindings (variables, multi-functions, struct constructors, etc.) and types. This document outlines the various kinds of scopes and how they interact.



### Type Scopes

Currently, there are two kinds of **type scopes** in Lore:

- The **registry type scope** contains all global named types, including all traits, structs, and type aliases.
- A **function type scope** contains the type variables declared in a function's `where` clause. 

The function type scope **shadows** the registry type scope, so if you declare a type variable `Apple` in a function, any type named `Apple` in the registry type scope won't be accessible from that function.



### Binding Scopes

Bindings are local and global variables, multi-function, and struct constructors. Currently, there are three kinds of **binding scopes** in Lore:

- The **registry binding scope** contains all multi-functions and struct constructors. If a multi-function and struct constructor share the same name, the multi-function is preferred.
- The **function binding scope** contains the parameters declared in the function's parameter list.
- **Block binding scopes** contain all local variables declared in the given block. Block binding scopes nest indefinitely.

These scopes **shadow** each other, with the block binding scope being the most preferred scope.

