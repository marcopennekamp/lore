# Types

Lore defines a small set of type constructors that can be used to declare new types.


## Function Types

A function type `A => B` is the type of an anonymous function with input `A` and output `B`. The `=>` symbol is right-associative, e.g. a function type `A => B => C` is the same as `A => (B => C)`. This allows us to support currying.


## Tuples

A tuple type `(A1, A2, ..., An)` is the product type of types `A1` through `An`. The values of a tuple are called elements. The ith element can be accessed as an attribute `_i`. 

Elements may be named, in which case the type is denoted as `(a1: A1, a2: A2, ..., an: An)`. Instead of accessing the ith element with `_i`, it is accessible via `ai`. Such a type is called *named* tuple.


## Unions





## Nominals

A nominal is a type with a single inhabiting value of the same name. In combination with sum types, nominals can be used in various useful ways, for example to implement enumeration-style concepts found in other languages or as keys in a hashmap. 

A nominal can be defined as follows:

    nominal Red

They can also be defined in-place, for example in sum types:

    type Color = nominal Red | nominal Green | nominal Blue


### Application: Enumerations


### Application: Status Handling









## Classes



