# Introduction

**Lore** is a general-purpose programming language featuring multi-functions, a type-safe entity-component system, and intersection and semantic types. We explore the combination of **experimental features** that come together to form a sum greater than the parts. These are:

- **Multi-functions** are dynamic dispatch functions that are completely independent from classes and dispatch on more than one parameter. They are intrinsically interesting, but they also tie together the other experimental features.
- **Intersection types** used with **semantic types** and **component types** are the vehicles that allow incredibly interesting programming techniques in conjunction with multi-functions. These techniques include function specialization based on semantic types, type augmentation at runtime, and decoupling functions from specific classes by referencing their components instead. We hope that this is especially useful for game development, but think it's useful in most programming situations.
- **Components** are class entities that are reflected as *parts* of a class in the type system. An object of a class can be viewed as its part in the form of a single component type (or multiple with intersection types). This allows you to take the intersection type of two components, for example, and implement a multi-function for it. This multi-function would then accept any objects that have both components as their parts, no matter the base type of the object or whether additional components are also present in the object. The multi-function has a special case where an object needs to have an additional component? No problem, just specialize the function that handles the two components and include the additional component. Components are great, because they allow you to program against incredibly flexible interfaces.

Apart from these core features, we also have concepts and ideas in mind for modern programming features that other languages are lacking, or features that tie especially well into Lore. 



### Origins

Components and multi-functions were **inspired by entity-component systems** and other concerns in game development. I was looking for a workable language that had a great type system to use with components and found none. I love the guarantees that a robust type system can give, so I was really disappointed.

The idea of **using intersection types and semantic types with multi-functions** came to me while working on my Bachelor's thesis. My thesis was about software synthesis, and the system I worked with relied heavily on semantic types to specify intricate details about values (e.g. that a list is sorted, and much more complicated notions). I suspect that this semantic specification is also great for specializing functions in a multiple dispatch context, which is one thing I want to find out by working on Lore.



### Minimum Viable Language

At this early stage of design and development, my goal is to **create a minimum viable language** that satisfies the **most important language goals**. While I have lots of ideas about what kind of quality of life features ought to be included in a modern programming language, the core ideas—ultimately what sets Lore apart from other languages—are more important. As I am a one-man team working on this project sporadically, my goal is to create a minimum viable specification and compiler that is just one thing: usable.

Hence, here is a **list of features** that will make up the minimum viable language; anything beyond that is fluff—happy fluff, however—that might be added at a later time.

On the **language** side:

- **Types:** Intersection types, sum types, product types, list types, map types, class types, component types, label types, abstractness, type inference.
  - We are excluding function types for now, but will add them very soon after a minimum version has been achieved.
  - Notably, we are excluding less important types such as singleton and envelope types.
- **Functions:** Multi-functions, function declarations, multiple dispatch, compile-time constraints, callee fixing.
- **Classes:** Properties, fields, constructors, mutability, component declarations, `owned by` declarations, simple polymorphism, abstract classes.
- **Expressions:** Literals and value constructors (numbers, strings, booleans, tuples, lists, maps, instantiation), very basic operators (such as addition; not even list operations), blocks, multi-function calls, conditional expressions, repetition, property access, variable declarations and assignments, return.
  - Notably, we are not yet supporting the definition of anonymous functions.
  - In the long run, almost all symbolic operations will be backed by multi-functions. For example, the comparison operator can be simply defined as a function. But to keep it simple for now, we will define a basic set of symbolic operations and then require standard multi-function calls. So, for example, comparing two strings would mean calling `areEqual(str1, str2)`.
- **TODO:** One **big question** is how far we'll support Javascript interop in the MVL. We would have to introduce dictionaries (values and type), dynamic types (as a type called Dynamic or alternatively when types are omitted from declarations), and duck typing.
- **Not supported:** Namespacing/modules, import/require, class/function/field visibility, dynamic specialization and generalization, pattern matching.
  - For this basic version of Lore, we will just compile all files in the source dictionary together and then invoke a main function. We won't yet support namespaces or imports.
    - **TODO:** We should reconsider whether we shouldn't support basic namespaces. (Maybe even without imports.) The problem is that we have some Lore types and functions which really should be part of a namespace, such as `range` (could be part of a `collections` namespace, or the `lore` namespace) and `println` (could be part of a `lore` namespace).
  - Dynamic specialization and generalization: This is a super-powerful feature which will probably be a headache to implement, so I'm leaving it out of the first version for now. It's better to focus on it once a base has been established.

On the **implementation** side:

- **Parser**
- **Abstract Syntax Tree**
- **Correctness checks** for multi-function constraints, types, and anything else that needs checking.
- **Transpiler** from the AST to Javascript.
- A **Javascript run-time** that handles multiple-dispatch.