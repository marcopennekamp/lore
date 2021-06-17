Outstanding questions:

1. How do we handle the naming of components which have type parameters? Still use the simple name and check uniqueness irrespective of type arguments? Disallow parameterized components altogether?
2. Thoughts on declared type LUBs:
    - Now that we have schemas, we have to check whether types e1 and e2 assign the same type variables for their most specific common supertype. Since type variables of a child type do not have to correspond 1:1 to those of a parent type, we have to follow the "call chain" of supertypes until we arrive at the desired schema, for both types, while substituting the right types. Only if we arrive at equal type arguments to the LUB is it actually a common supertype. Since there may be multiple such types, we have to check it for each independently, and then create an intersection type (or Any) with the fitting supertypes.
    - I don't think the above works so naively. Take the following type hierarchy:
      ```
      trait A
      trait B[T] extends A
      struct C[T] extends B[T]
      struct D[T] extends B[T]
      ```
      The LUB of `C[Int]` and `D[String]` should be `A`, but the algorithm as envisioned would return `Any`, because the declared type hierarchy returns only `B` as the most specific common schema. The way to fix this is to instantiate a subgraph of the declared type hierarchy with the given type arguments.
