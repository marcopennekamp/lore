# Lore

Lore is a general-purpose programming language with multi-functions, component-based classes, intersection types, and semantic types.  


### Vision

Lore is a language that explores how well a few experimental features come together to form a sum that is greater than its parts. These experimental features are:

- **Multi-functions** are dynamic dispatch functions that are completely independent from any class types and dispatch on more than one parameter. They are intrinsically interesting, but they also tie together the other experimental features.
- **Intersection types** used with **semantic types** and **component types** are the vehicles that allow incredibly interesting programming techniques in conjunction with multi-functions. These techniques include function specialisation based on semantic types, type augmentation at runtime, and decoupling functions from specific classes by referencing their components instead.
- **Components** are parts of classes that are reflected, as actual parts, in the type system. Each object of a class can be viewed through a single component type (or multiple with intersection types). This allows you to take two components, for example, and implement a multi-function for it. Some objects have a third component with more information? No problem, just specialize the function that handles the two components. Components are great, because they allow you to program against incredibly flexible interfaces. 

Components and multi-functions were inspired by entity-component systems and other concerns in game development. I was looking for a workable language that had a great type system to use with components, and found none. I love the guarantees that a robust type system can give, so I was really disappointed.

The idea of using intersection types and semantic types with multi-functions came to me while working on my Bachelor's thesis. My thesis was about software synthesis, and the system I worked with relied heavily on semantic types to specify intricate details about values (e.g. that a list is sorted, and much more complicated notions). I suspect that this semantic specification is also great for specializing functions in a multiple dispatch context, which is one thing I want to find out by working on Lore.

Apart from these core features, I also have concepts and ideas in mind for modern programming features that other languages are lacking, or features that tie especially well into Lore. 


### State of the Project

I am currently writing the language specification. There is no Lore compiler, yet. We have a small experimental compiler that is used to test several multiple-dispatch concepts and implement core algorithms. 

We are planning to support Javascript as a target, because of its ubiquity. JS has the advantage that it is easy to interface with and each have an excellent VM/JIT implementation available for sufficient performance.
