# Multi-Functions

**Multi-functions** are an exceedingly important feature of Lore. They allow specialising a function via any of the parameters, thereby allowing dynamic dispatch over more than one argument. This feature is also called **multiple dispatch**.

*Definition.* A **multi-function** is a set of all functions that share the same name. Such an individual function is also called an **instance** of its multi-function. 

Multi-functions can be **invoked** like ordinary functions. The actual function being invoked is chosen at runtime as the function that is most specific in the set of functions that can be invoked with the actual (dynamic) argument types. That is, we consider functions of which the argument types are subtypes of the parameter types and then choose the most specific function. We call this kind of invocation **multiple dispatch**.

Multi-functions are useful because they allow functions to be implemented with varying levels of specificity. They lend themselves well to a varying ensemble of features and concerns:

- **Single Dispatch** is supported natively, as multiple dispatch subsumed single dispatch.

- **Intersection Types** can be used to define a sufficiently specific type for a given operation without naming concrete value types. For example, we can define a function that accepts the intersection of two component types, thereby opening that function to any object that has these two components.

  Furthermore, varying degrees of increasingly specific intersection types can be used to **specialize** a function in different ways. This sufficiently specific type can be used to implement a function which would otherwise have been confined to a class hierarchy in single-dispatch languages.

- **Dynamic Specialization and Generalization** of values can be used to specialize or generalize the semantic type of a value *at runtime*. Since the actual function being called is chosen at runtime when calling a multi-function, we can write functions that implement an operation for a given temporarily specialized argument.

- **Extendability** is improved by the ability to define multi-functions *across* files and compilation units. This supports features such as C#'s extension methods or Scala's implicit classes in a concise and native way.

**In this chapter,** we will look at the syntax of function declarations and define functions and multi-functions. After laying out the basics, we will define the rules of multiple dispatch and examine constraints and edge cases. We will see how intersection types, dynamic specialization and generalization, and extension methods can be used as sugggested above.
