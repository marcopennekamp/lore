# Functional Constructors

Constructors in Lore are, at their most fundamental level, ordinary functions that can be called to create a value of a specific type.


## Basic Definitions

### Constructors

A constructor is any function `(T1, ..., Tn) => T` with the purpose of constructing a value of type `T`. We try to strip down all the shenanigans that surround constructors in other languages and better integrate them into the functionally-friendly side of Lore.

In practice, constructors may be defined with the `constructor` keyword:

    constructor FromFahrenheit(v: Float): Temperature = Temperature((v + 459.67) * (5/9))

The keyword has no special semantic meaning, but it signals the intent of the programmer. Note that a constructor is **not** an instance method of the type it constructs, but rather an independent function.

It is perfectly feasible to write a function that returns a constructor function:

    function TemperatureFactoryFactory(c: Country): Float => Temperature

TODO: Where do you declare constructors if they are not an instance method?


### Root Constructors

Each constructible type `X` defines a *root constructor* `X: (T1, ..., Tn) => X` for some types `T1` through `Tn`. You **must** use this constructor to create instances of `X`. Any other constructor function will use the root constructor to create objects of `X`.

It is generated as follows: Attributes (including components) are added to the constructor's parameter list in the order that they appear in inside the type's body. Each parameter adopts the appropriate type of the attribute. A component attribute of type `C` will result in a parameter type `Bindable[C]`, because component objects need to be bound to the owner object. Default attribute values are added as default argument values to the root constructor.

For example, take the following class:

    class Skeleton extends Entity2D {
        component healthState: HealthState
        import {const health, heal, damage} from healthState
        private const boneCount: Int = 5
        ...
    }

The root constructor of `Skeleton` has the following signature:

    Skeleton(position: Bindable[Position],
             sprite: Bindable[Sprite],
             healthState: Bindable[healthState],
             boneCount: Int = 5)

The attributes `position` and `sprite` are derived from the `Entity2D` superclass. The default attribute value for `boneCount` ends up as a default argument value.


### Overwriting Root Constructors

It is sometimes inconvenient when private attributes are exposed through the root constructor, especially when it comes to attributes like `boneCount` in the example above. In such and other cases, you can overwrite the root constructor as follows:

    class Clock {
        component position: Position
        private mut time: Float = 0.0
    }

    object Clock {
        constructor Clock(position: Bindable[Position]) = this(position) // Note that the time parameter is assigned its default value.
    }

TODO: Can we find a better syntax than `this(...)` for this?


## Application of Constructors

### Partial Application
