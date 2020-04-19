# Compiler

[Previous File](05-expressions.md)

In this document, we outline the **design of the compiler**. It is prudent to consider the high-level design of the compiler on a conceptual level first, for three reasons: one, it will be easier for other developers to understand the structure of the compiler; two, while developing the compiler, we can always go back to these documents and periodically audit whether the implementation is still following a good direction; and three, I hope to make fewer mistakes coding the compiler when I have considered the high-level design first.



## Error Reporting

Undoubtedly, one of the most important aspects of a good compiler is the ability to provide clear **error handling**. In this sense, *the first version of the Lore compiler will be crap*. (I want to make it work before I "make it clear.")

Nonetheless, I want to offer some preliminary thoughts on error reporting. First of all, errors should be **collected** on a fragment-by-fragment or declaration-by-declaration basis (in later phases). The compiler should stop when a phase has found errors, but it shouldn't report one error and stop the whole compilation. Hence, we cannot work with expressions. Rather, errors should be **accumulated for each fragment**, which can then be reported when a phase ends.



## Phases

The **phases** of the compiler are "themed" steps that either transform one **representation** into another representation, or validate a representation. For example, the parsing phase transforms the source code into an AST.



#### Representation 1: Source Code

At the beginning of the compilation chain lies the **source code**. Lore is aiming to be **file-system agnostic**, that is, not that it works on any file system, but that it shouldn't matter whether the compiler is fed files or something else. The basic self-contained unit of source code is called **fragment**. A fragment is, often, a file, whose source is read by the parser, but this doesn't always have to be the case.

Fragments can sometimes be processed independent of each other, maybe even in **parallel**. The same applies to function declarations, type declarations, and so on. We will design the 

Hence, starting off, the **initial representation** of Lore is text in the form of valid, invalid, or partially valid source code.



#### Phase 1: Parser

Each fragment is **parsed** using fastparse into an AST representation. As of now, error reporting in this stage isn't good, as fastparse's error reporting is quite unwieldy out of the box. We will need to spend more time to make it usable.



#### Representation 2: ASTs

A set of **ASTs**, each bundled within a fragment data structure. Each node of the AST has **index** information which determines where the node *started* in the source code.



#### Phase 2: Declaration Resolution

All declarations, across multiple fragments, are added to a **Declaration Resolver**, which is meant to first learn about all available declarations across fragment boundaries. This is practically necessary because type and function declarations depend on other type declarations, such as a class depending on a superclass that it extends, and these declarations may appear out-of-order in the same fragment or even arbitrarily across multiple fragments.

Once all names have been registered with their associated ASTs, we have to **resolve declared types hierarchically**. During the dependency resolution step, types are added to a graph with directed edges signifying relationships between types (such as subtyping). We sort this graph topologically and then start to resolve types beginning with the root types, i.e. those which don't depend on any other types.

Class **members** have to be handled with special care: For example, let's say we have the following classes `A` and `B`:

```
class A {
  b: B
}

class B {
  a: A
}
```

Regardless of whether this is even instantiable, it is undoubtedly a valid piece of Lore code. The dependency graph will have directed edges from A to B and from B to A. Hence, it would not be possible to sort the graph topologically without ambiguity. Thus, we **defer the resolution of property types**. We declare property types as lazy, so the ClassType instances for A and B can be created without resolving B and A as property types. Then, the first time a property type of A or B is accessed by the compiler, the types are resolved just in time.

In other cases, we might find **cyclical subtyping relationships** (`A extends B` and `B extends A`). These are, obviously, not valid, and so the dependency graph should recognize these cycles and raise an appropriate error.

Once declared types have been resolved, we can build the **Definition** instances for all high-level declarations and register them in the **Registry**. A Definition is a smart representation of a declaration's AST. It carries all information in processed forms, such as Type instances instead of TypeExprNodes, except for function and constructor bodies: we keep working with these (partial) ASTs until the representation is transpiled to Javascript. Each function/constructor body AST is part of the respective Definition.



#### Representation 3: Definitions

The chief representation of the Lore program is now a set of **Definition** and **Type** instances, both held in the **Registry**. Function/constructor bodies are embedded as ASTs into their respective Definitions. Functions are ordered into their **multi-function** structure, which are independent of fragment boundaries.



#### Phase 3: Types and Constraints

This phase **types** all definitions and verifies specific **constraints:** 

- It verifies constraints about **classes and entities**, such as classes not being able to extend entities.
- It deduces **member types** and checks that all members (properties and components) are valid according to all constraints (inheritance, overriding, etc.).
- It deduces **types in function/constructor bodies** (all nodes) and ensures that function/constructor declarations adhere to type boundaries. This step includes typing and checking **expressions** for type correctness and other possible constraints. 
- It also checks constraints over **multi-functions** such as the input abstractness constraint and totality constraint.



#### Representation 4: Typed Definitions

At this point, all Definitions, especially functions and constructors, have been **typed**. Expressions are still represented in their AST form, although nodes have been augmented with type information.





