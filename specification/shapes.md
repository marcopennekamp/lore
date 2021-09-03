# Shapes

A **shape** is a partial view on structured data, with the option for ad-hoc representation. A shape consists of a set of **immutable properties**, much like a struct. However, a shape does not define a constructor of any kind and, crucially, has **no name**. If properties agree, a shape type can describe a struct, making the struct a **subtype** of the shape. **Shape values** can also be created in place, without specifying a type. Such an ad-hoc representation is similar (especially in convenience) to maps, dictionaries, or objects from dynamically typed languages. Shapes offer the power of structural typing.

At run time, multi-function calls are **dispatched** based on the actual property types contained in the shape value, or struct if the property is open. This allows a Lore programmer to specialize functions based on actual property types, effectively enabling styles such as **component-based programming**.

###### Syntax Example

```
type Positioned = %{ x: Real, y: Real, z: Real }
type Dimensioned = %{ width: Real, height: Real, depth: Real }
type Spaced = Positioned & Dimensioned
```



### Shape Values

Shapes can be directly constructed as **values**. This comes in handy for ad-hoc data structures, such as multi-part values returned from a function, or options passed into a function or constructor. Such values are **not** structs. All properties of a shape value are immutable.

###### Syntax Example

```
type Options = %{ show_teeth: Boolean, volume: Real  }

func bark(options: Options): String = ...

act test()
  bark(%{ show_teeth: true, volume: 80 })
end
```



### Type Semantics

Shape types are *structural types*. A struct or shape type A is a **subtype** of another shape type B if A contains and subtypes all properties of B.

###### Example

```
type Positioned2D = %{ x: Real, y: Real }
type Positioned3D = %{ x: Real, y: Real, z: Real }
// Positioned3D is a subtype of Positioned2D, but not vice versa.
```



### Run-time Type Semantics and Open Properties

The run-time type of a **shape value** is created from the actual run-time property types present during value creation. This ensures that run-time dispatch takes actual property types into account, but requires that each instantiation of a shape value also creates a new shape type. Such operations are costly, but are also the only way to offer maximum flexibility for shape types.

The run-time type of a **struct property** depends on whether the property is `open`. If the property is open, the actual run-time type of the property value at the point of instantiation becomes part of the run-time struct type. Otherwise, if the property is static, the *compile-time type* of the property is part of the run-time struct type instead.

Open properties are generally slower, because they lead to more difficult run-time type mechanics. 

###### Example

```
func free(cage: %{ content: Animal }): Animal = cage.content
func free(cage: %{ content: Tiger }): Nothing = error('Are you insane?')

struct Blackbox(content: Animal)
struct Whitebox
  open content: Animal
end

free(%{ content: fish })   // --> returns the fish
free(%{ content: tiger })  // --> throws the error
free(Blackbox(tiger))      // --> returns the tiger, oh-oh!
free(Whitebox(tiger))      // --> throws the error
```

`Blackbox` doesn't know what the run-time type of `content` is, because the property is static. Hence, calling `free` with a black box filled with a tiger results in the first function being called. In contrast, `Whitebox` lets `free` make the smart decision of not releasing the tiger, because the open property provides enough information at run-time.



### Component-based Programming

One way to build programs with a healthy level of abstraction is **component-based programming**. The idea is that data consists of multiple components which can each provide already implemented functionality. For example, an **entity** called `Hero` may have a component `Position` and a component `Shape`. There may be functions such as `move` to manipulate position and a function `render` that requires both a `Position` and a `Shape` to work.

We can support such **entity-component systems** natively (and especially with type safety) in Lore using structural typing. The following code implements the constellation of Hero, Position and Shape:

```
struct Position
  mut x: Real, mut y: Real, mut z: Real 
end
type +Position = %{ position: Position }

act move(entity: +Position, distance: Real, direction: Vector3)
  // calculate new x, y and z coordinates...
  entity.position.x = x
  entity.position.y = y
  entity.position.z = z
end

struct Shape
  width: Real, height: Real, depth: Real 
  model: Model 
end
type +Shape = %{ shape: Shape }

act render(entity: +Position & +Shape)
  // use entity.position and entity.shape to render the entity...
end

// The `extends` is not strictly necessary, but provides additional compile-time safety should either
// +Position/+Shape or the corresponding properties change unexpectedly.
struct Hero extends +Position, +Shape
  position: Position  // Note that position does not need to be open since structs can't have subtypes.
  shape: Shape
end
```

The remarkable benefit of this approach: any struct or shape that contains a property `position: Position` and `shape: Shape` can be used as an entity for the  `move` and `render` functions. We can conceive any number of additional entities that can just as much use these already existing functions. This enables **generic programming over partial structures**, i.e. entities and components.

**Specialization** is another great aspect of this programming model. Imagine we want to implement an additional `render` function for those entities that not only have `Position` and `Shape`, but also `Color`. We can simply write a second function:

```
act render(entity: +Position & +Shape & +Color)
  // use position, shape AND color to render the entity...
end
```



### More on Open Properties

The ability to dispatch on property types effectively turns a struct type into a *family* of possible types. For each combination of actual run-time property types, a new struct type has to be created if we want to support shape dispatch. This has the following drawbacks:

1. Much like with shape types, a struct type would have to be created for each instantiation of a new struct value. This is a **costly** operation, likely more expensive than creating the struct value itself. Furthermore, the more type values exist for the "same" struct type, the more type instances have to be taken into account during multiple dispatch. Non-interned but equal struct types require a costly equality check each time the dispatch cache is hit. Struct types with different run-time property types have to produce different hashes, with each different type instance taking a new place in the dispatch cache. This possibly bloats the dispatch cache of countless methods.

2. Multiple dispatch suffers from the nasty problem that **subtyping and type equality** are of extreme importance to generic functions. Hence, it is quite important that the usual norms of subtyping and type equality are not broken by accident. Making all struct types dependent on run-time property types without giving the programmer control over the matter is a dangerous, implicit, accident-producing feature.

   Take the following example. If we assume that `Coffin`'s type takes into account the run-time property type of `nail`, we have it quite trivially and on good account that the two coffins compared in the last line don't have equal types. Even worse, they are not even subtypes of one another. Since the programmer probably doesn't expect two types of the same name (in a mostly nominal typing context) to *not* be equal, this might lead to a lot of confusion. Unless the programmer specifically *states* that "yes, I want run-time struct types of the same name to vary," it's probably not a good idea to trip her up like this.

   ```
   @where A, B
   func equal_types(A, B): Boolean = false
   
   @where A
   func equal_types(A, A): Boolean = true
   
   trait Nail
   struct FancyNail
   struct BloodyNail
   
   struct Coffin(nail: Nail)
   
   act main()
     let fancy = FancyNail()
     let bloody = BloodyNail()
     let coffin_fancy = Coffin(fancy)
     let coffin_bloody = Coffin(bloody)
     
     equalTypes(coffin_fancy, coffin_bloody) // --> false
   end
   ```

From the first point, it is clear that we cannot just have all properties marked as "dispatchable". There is the opportunity for different **optimizations**, but none of them are straight-forward. If the compiler could analyze this problem perfectly, it would only mark the properties as dispatchable that are actually involved in structural dispatch. But deciding whether a run-time property type is used for run-time dispatch is impossible. It likely reduces to the halting problem.

Taking the second point into account, assuming "open" properties to be the default not only seems detrimental to performance, but also seems like an **unsound design decision**. There are legitimate cases in which we want to have run-time dispatch on property types, but there seem to be at least as many cases in which we *don't* want to differentiate between run-time property types.

So in the end, the best option is to make run-time variance of property types **explicit**. One must thus declare a *struct* property as `open` if it should be able to partake in run-time structural type dispatch. If a property is not marked as `open`, its type as known by the struct type is always the property's compile-time type.

```
struct Position3D(x: Real, y: Real, z: Real) extends Position

struct Hero
  health: Health
  open position: Position
end

type +Position = %{ position: Position }
type +Position3D = %{ position: Position3D }

act move(entity: +Position)
  ...
end
act move(entity: +Position3D)
  ...
end

act test()
  let hero = Hero(Health(), Position3D(0, 0, 0))
  
  // Dispatches to the second move function, since position is an open property.
  // If position wasn't open, the call would dispatch to the first move function.
  move(hero)
end
```

