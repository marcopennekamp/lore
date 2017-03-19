# Functional Constructors

Value constructors in Lore are, at their most fundamental level, ordinary functions that can be called to create a value of a specific type.


## Basic Definitions

### Constructors

A constructor is a function `(T1, ..., Tn) => T`, with `n` representing any number of arguments.

In practice, constructors can be defined with the `constructor` keyword:

    constructor FromFahrenheit(v: Float): Temperature = Temperature((v + 459.67) * (5/9))
    
The keyword has no special semantic meaning, but it signals the intent of the programmer. Note that a constructor is **not** an instance method of the type it constructs, but rather an independent function that just so happens to construct the desired type.
    
It is perfectly feasible to write a function that returns a constructor function:

    function TemperatureFactoryFactory(c: Country): Float => Temperature


### The Root Constructor

Each constructible type `X` defines a *root constructor* `X: (T1, ..., Tn) => X` for some types `T1` through `Tn`. We must use this constructor to create instances of `X`. Any other constructor function will use the root constructor to create objects of `X`.

In the case of classes, the root constructor has to be defined manually.

In the case of structs, it is generated as follows (unless specified manually): For attribute types `A1` through `An`, let `(T1, ..., Tn) = (A1, ..., An)` in the root constructor type. The constructor is then defined by assigning the argument values to their respective fields.


## Application of Constructors

### Partial Application


### Dependency Resolution


