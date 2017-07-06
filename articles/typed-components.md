# Typed Components

In this document we introduce one of the core features of Lore: A statically typed composition system with particular focus on entity-component systems used in game development.



## Motivation

In notable engines with entity-component systems such as Unity, components are added to and possibly removed from an entity at runtime. Both the engine editor and the compiler of the scripting language thus can't guarantee that an entity has a specific component. This has profound implications on the code quality of a game:

- **Correctness:** Static type systems are a powerful feature of many programming languages. They allow the programmer to declare which expressions may produce which kinds of values and how these relate to each other. This helps the compiler to disallow programs that might be unsound – for example, programs where an undefined method is used or programs where a function receives an argument that has the wrong type. We can think of a render function that can only render entities with the `Sprite` component. If entities are typed at runtime, we can at best guess whether an entity will have the required component to make it renderable. In contrast, using a suitable static type system will give us stronger error checks and correctness guarantees.
- **Maintainability:** Removing components from an entity during refactoring can prove tedious if you can't trace the places in your codebase where the missing component is used. Especially in game development, where code is scarcely covered by tests, such a refactoring might lead to bugs that aren't found until the game has already been released. In contrast, using statically typed components would prompt the compiler to display an error for each misuse of the now missing component.
- **Simplicity:** Often, you have to get the component before you can use it. In Unity, for example, you have to use the [`GetComponent`](https://docs.unity3d.com/ScriptReference/GameObject.GetComponent.html) method of a game object to get one of its components. This also requires you to remember the type of the component. Even worse, `GetComponent` may return null if the component is not attached to the entity, bringing us right back to the problem of correctness, or, alternatively, added complexity through null checks. In contrast, statically typed components give us direct access to a component: `player.GetComponent<Stats>().health` becomes `player.Stats.health`.
- **Code Completion:** Code completion in editors can sometimes rely on good heuristics, but a static type system makes finding the right names and signatures for code completion both easier and more reliable.

We believe that these reasons pose ample incentive to explore the idea of using a static type system for entity-component systems. We also believe that these features need to be added on a language level, because treating the components as a design pattern will lead to issues of usability and expressiveness.



## A Note on Generality

One of our main goals with Lore is to create a language that brings static typing to entity-component systems commonly used in games. However, we believe that the fundamental concept of composition is worth investigating, both in contrast to and in cooperation with more traditional OOP approaches.

To achieve that, we will keep the language as general as possible, so that it may be used in contexts apart from game development without sacrificing usability or expressiveness.



## Components

### Definition

Components can be declared in any class where attributes can be declared (although without a name):

    class A {
      component Part
    }

Thus, the type `A` has the component `Part`. The component can be accessed like an attribute, `a.Part`, with the type name as the accessor name.

The type `A` satisfies the typing `A +Part`, which means that `Part` is a component of `A`. The `+` (pronounced "has") type qualifier is defined in the following sense: Let C<sub>1</sub>, ..., C<sub>n</sub> be the components of a type `A`. Then `A` satisfies the type <code>A +C<sub>1</sub> ... +C<sub>n</sub></code>.


### Component naming and multiple components of the same type

Note that the name of a component is the same as the name of its type. We chose this approach because the has-relation doesn't distinguish names. Having two components of the same type was distinguishable inside the class definition (since those components would be different attributes), but not in the type itself. We would have had either to add names to the `+T` part of a type or remove "named" components. The former option would have made passing around components virtually impossible: A hypothetical type `+position: Position` would not have satisfied a type `+pos: Position`. Because of this, we chose the latter option.

This opens up another question: Suppose I have a `Wheel` and want to add 4 of them to a `Car`. You can't simply add 4 components of the same type. The solution would be to use some kind of wrapper type, for example:

* `(Wheel, Wheel, Wheel, Wheel)` (verbose, hard to use)
* `List[Wheel]` (doesn't encode the number of wheels)
* `FixedList[Wheel, 4]` (just an idea...)
* `WheelSet` (a class with 4 normal attributes; probably the best solution, but hardest to set up)


### Component Instantiation

Since components do not have special types, but yet they may interact with their owner value, we need two ways to create values: Standard instantiation and component instantiation. This section deals with component instantiation.

Components can't be initialised without being bound to an owner. To create a bindable component, the component constructor needs to be called with the `@` prefix: `@Position(1, 2, 3)`. The outcome is a value of type `@C` (speak "bindable C"), where `@` is a container type that prohibits access to the `C` value.

Similarly, an owner type's constructor will ask for a `@C`. Such a compiler-generated constructor is the **only** way to free an unbound component. The compiler will ensure that the owner and component are properly bound to each other.

Of course, there is no way to create a `@C` from an already existing `C` value. Since types with components require a `@C`, not a `C`, no already bound component can possibly be bound to two different objects.

TODO: For the bindings to work properly, we need to invalidate an `@C` value once it has been passed to some function, i.e. we need to make sure that it's only unpacked at most once. We may want to have a look at Rust and borrow ideas from the borrow checker. 

Types that use their owner may only be instantiated through component instantiation. This is determined by the compiler. Values of other types may exist in either of the two modes.

TODO: Starting to use the owner value (even by accident) could have effects not only on the implementation of a class, but on all the places where it is used in a non-component capacity. We should consider making the "component-only state" a bit more explicit (i.e. introducing a qualifier that needs to be present for the owner type to be usable). This would still allow us to use classes as components that don't use an owner value, but add a fail-safe mechanism to classes which may need access to their owner value at some point in the future.


### The Owner Declaration

Some components may depend on other components. For example, an AI component may rely on a position component. We want to provide a language-native way to deal with such dependencies.

Since every component belongs to exactly one object, called the *owner*, at all times, a component may access its owner. In addition to that, the component may **require** the owner to be of some type `T`. This allows the component access to *other* components that the owner type also consists of. Refining the owner type can be achieved in the following way:

    component A {
      owner :: T
      ...
    }

We want to illustrate this with the following example:

    component LemmingAI {
      owner :: +Position
      action walk () :: Unit -> Unit = {
        ...
        owner.Position.translate(x = 1) // Always forward!
      }
    }

Here, `LemmingAI` requires its owner to have a `Position` component, which allows us to access said component.


### Importing from a Component

TODO: Scrap this feature? Doesn't seem to be very useful without being able to declare standard import sets.
      Import sets could be added through an export feature.

In some cases, it's useful to be able to access a component attribute or function directly from the owner. Introducing an operator that would cover all component names is however convoluted, because it would lead to obscure naming issues. For example, see the spacetime-clock example below. It's therefore advisable to construct the component-context namespace **explicitly**.

You can import names like this:

    component Position import {x, y, z}

The names will be available for use just like direct declarations in the owner.

Whether renaming imported names will be supported in the future is up for discussion. Right now we think that if a name needs to be changed for an import to be unambiguous, one should rather access the member through the component instead.

#### Immutable Imports

When importing attributes, the import adopts the mutability of the component attribute. You can also force an import to be immutable by using the `const` qualifier.

    component HealthState import {const health}


### Adding and Removing Typed Components

While adding components to and removing components from an entity can be achieved even with typed components, we can't update existing entity types at runtime to compensate for an added or removed component. However, behaviour such as this is exactly what we want to disallow with typed components **most of the time**. We don't want to accidentally pass an entity that does not have a Sprite to the `render(entity :: +Position +Sprite)` function. If we use this code with our entity, surely we should not be able to remove the Sprite component.

However, in some cases it is beneficial to have a more dynamic entity type. Maybe some entity only really has some component in certain cases. In that case, we also probably want functions that are only executed if the specific component is currently part of the entity.

Solution: dynamic entity types, monads / syntactic support for runtime component resolution


### Referential Immutability

Component references are required to be immutable because of the following reasons:

1. Subtyping rule (3) allows component subtyping. If we want to replace a component, we must be sure that we replace it with a valid type. This is not possible if we are potentially dealing with a subtype of a component. The solution is to disallow replacement of components, i.e. make components immutable.
2. Since components may declare dependencies and components may be passed around like other ordinary values, it is sensible to require that a component is always tied to a specific entity. Allowing mutable components may lead to bugs that are hard to detect when a component of an entity is replaced by another component, but not all references to the component are updated.

We may suggest a workaround for component immutability in the future, for example container types, but in general, components should be viewed as referentially immutable.

Note that, while the **reference** is immutable, the component **itself** does not have to be immutable. You can, of course, still model changing state in Lore, but that change needs to be applied inside the component, not by replacing a component.



## Component Theory

This section is more theoretical and might not be easy to understand for some readers. While the topics discussed here are important, it is not a requirement for using components in Lore.


### Implementation with Intersection Types

TODO: Describe that types in Lore are fundamentally intersection types (such as `List[Int] SortedList +Size`).

The component type system defined here is a subset of type systems that support intersection types. Namely, we have a single parametric type `Has[C]` that signals that a component `C` is present. For a type <code>A +C<sub>1</sub> ... +C<sub>n</sub></code>, the corresponding type using intersection types would be <code>A &#8745; Has[C<sub>1</sub>] &#8745; ... &#8745; Has[C<sub>n</sub>]</code>. For example, a type `Player +Position +Sprite +Stats` would become <code>Player &#8745; Has[Position] &#8745; Has[Sprite] &#8745; Has[Stats]</code>.

This view on component types will provide us with a theoretical basis that we can use should we need it.


### Subtyping

Since Lore also supports subtyping, we need to establish subtyping rules that touch types with components. Fortunately, we can borrow subtyping rules from a type system that supports [intersection types](https://www.cis.upenn.edu/~bcpierce/papers/thesis.pdf).

This gives us the following rules concerning components:

1. `A +C <: A` – Adding a component to a type `A` still allows it to be treated as just `A`. Note that `A` may be a type that includes further components not listed here.
2. <code>B <: A +C<sub>1</sub> &#8743; B <: A +C<sub>2</sub> => B <: A +C<sub>1</sub> +C<sub>2</sub></code> – Subtyping respects intersection of component types. This generalizes to a variable amount of components.
3. <code>A <: A' &#8743; C <: C' => A +C <: A' +C'</code> – A subtype of `A'` is still required to provide an implementation for a component `C'` of `A'`. However, this implementation may be a subtype of `C'`.

Please let me know if I missed a rule.

Note that rule (3) implies that `Has[C]` as defined above is covariant in `C`, which means that if `C <: C'`, we have `Has[C] <: Has[C']`. This makes intuitive sense, as the following example will suggest:

    interface Temperature {
      const kelvin :: Real
    }

    class CelsiusTemperature implements Temperature {
      const celsius :: Real
      override kelvin = celsius + 273.15
    }

    class EuropeanHuman {
      component CelsiusTemperature
      ...
    }

    class Freezer {
      const objects :: List[+Temperature]
      action update () :: Unit -> Unit = ... // For all objects, update temperature.
    }

Even though a `EuropeanHuman` only has a temperature in celsius, we want to put it in the freezer. We need rule (3) to justify the subtyping `EuropeanHuman +CelciusTemperature <: +Temperature`.

Because of rule (3), component references may not be changed from outside the class that defines the component. To see why, consider the following scenario: We want to implement the method `update` of `Freezer` from the example above, so we want to replace the temperature components in each object. But we don't know the actual type of the component, just that it is a subtype of `Temperature`. If we were able to replace it with, say, `FahrenheitTemperature`, all the `EuropeanHumans` would complain about a temperature system that is unknown to them, or more precisely, the runtime would not be able to assign an object of `FahrenheitTemperature` to a `CelsiusTemperature` variable, because Fahrenheit is certainly not Celsius.

TODO: Component subtyping leads to a diamond problem. Consider a `WorldCitizen` with two components: `CelsiusTemperature` and `FahrenheitTemperature`. Which component is chosen if we want to assign it to a value of type `+Temperature`? Of course, we could simply throw a compilation error, but how do we solve the problem then? How can we put a citizen of the world into our freezer?


### Transitivity of the has-Relation

This section aims to show why the has-relation is not transitive.

In other words, suppose we have the following type:

    Part has P1 has P2

Here, `P1` is a component of `Part`, as is `P2`.

Now we have the following type:

    A has Part

The question is, does `A` satisfy the type `A +Part` or the type `A +Part +P1 +P2` by transitivity? Of course, the answer to that question decides how we interpret component hierarchies.

A brief argument can be formulated with intersection types. Let's consider the example above: `A +Part` vs. `A +Part +P1 +P2`. We have:

<pre><code>A &#8745; Has[Part] = A &#8745; Has[Part &#8745; Has[P1] &#8745; Has[P2]]
              &#8800; A &#8745; Has[Part &#8745; Has[P1] &#8745; Has[P2]] &#8745; Has[P1] &#8745; Has[P2]
              = A &#8745; Has[Part] &#8745; Has[P1] &#8745; Has[P2]
</code></pre>

We can see that components of components will be nested inside the type argument of `Has`. This syntactic approach strongly suggests that `has` should not be transitive.

We can also look at an example to suggest that a sensible has-relation should not be transitive. Consider the following types:

    class Space {
      const x :: Real
      const y :: Real
      const z :: Real
    }

    class Time {
      const seconds :: Real
    }

    class Position {
      component Space
      component Time
    }

    class Clock {
      component Position
      ...
    }

In this example, the time of the clock should not be the same as the time of the position. If `Has` was transitive, a clock would have the type `Clock +Position +Time +Space`, even though the time is exclusive to the position in spacetime and not the time on the clock.
