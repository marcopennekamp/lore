# Introduction

**Lore** is a general-purpose programming language featuring multi-functions, a type-safe entity-component system, and intersection and semantic types. We explore the combination of **experimental features** that come together to form a sum greater than the parts. These are:

- **Multi-functions** are dynamic dispatch functions that are completely independent from classes and dispatch on more than one parameter. They are intrinsically interesting, but they also tie together the other experimental features.
- **Intersection types** used with **semantic types** and **component types** are the vehicles that allow incredibly interesting programming techniques in conjunction with multi-functions. These techniques include function specialization based on semantic types, type augmentation at runtime, and decoupling functions from specific classes by referencing their components instead. We hope that this is especially useful for game development, but think it's useful in most programming situations.
- **Components** are class entities that are reflected as *parts* of a class in the type system. An object of a class can be viewed as its part in the form of a single component type (or multiple with intersection types). This allows you to take the intersection type of two components, for example, and implement a multi-function for it. This multi-function would then accept any objects that have both components as their parts, no matter the base type of the object or whether additional components are also present in the object. The multi-function has a special case where an object needs to have an additional component? No problem, just specialize the function that handles the two components and include the additional component. Components are great, because they allow you to program against incredibly flexible interfaces.

Apart from these core features, we also have concepts and ideas in mind for modern programming features that other languages are lacking, or features that tie especially well into Lore. 



#### Origins

Components and multi-functions were **inspired by entity-component systems** and other concerns in game development. I was looking for a workable language that had a great type system to use with components and found none. I love the guarantees that a robust type system can give, so I was really disappointed.

The idea of **using intersection types and semantic types with multi-functions** came to me while working on my Bachelor's thesis. My thesis was about software synthesis, and the system I worked with relied heavily on semantic types to specify intricate details about values (e.g. that a list is sorted, and much more complicated notions). I suspect that this semantic specification is also great for specializing functions in a multiple dispatch context, which is one thing I want to find out by working on Lore.
