# Types

Lore defines a small set of type constructors that can be used to declare new types.



## Product Types (Tuples)

### Definition



## Sum Types

### Definition



## Nominals

A nominal is a type with a single inhabiting value of the same name. In combination with sum types, nominals can be used in various useful ways, for example to implement enumeration-style concepts found in other languages or to function has keys in a hashmap. 

### Definition

A nominal can be defined as follows:

    nominal Red

They can also be defined in-place, for example in sum types:

    type Color = nominal Red | nominal Green | nominal Blue


### Application: Enumerations


### Application: Map-Keys



## Classes



