# Shapes

A **shape** is a partial view on structured data, with the option for ad-hoc representation. A shape consists of a set of **immutable properties**, much like a struct. However, a shape does not define a constructor of any kind and, crucially, has **no name**. If properties agree, a shape type can describe a struct, making the struct a **subtype** of the shape. **Shape values** can also be created in place, without specifying a type. Such an ad-hoc representation is similar (especially in convenience) to maps or objects from dynamically typed languages.

At run-time, multi-function calls are **dispatched** on the actual property types evidenced by the struct or shape value. This allows a Lore programmer to specialize functions based on properties, effectively enabling styles such as **component-based programming**.

###### Syntax Example

```
type Positioned = { x: Real, y: Real, z: Real }
type Dimensioned = { width: Real, height: Real, depth: Real }
type Spaced = Positioned & Dimensioned
```




### Shape Values

Shapes can be directly constructed as **values**. This comes in handy for ad-hoc data structures, such as multi-part values returned from a function or options passed into a function or constructor. Such values are **not** structs. All properties of a shape are immutable.

###### Syntax Example

```
type Options = { showTeeth: Boolean, volume: Real  }

function bark(options: Options): String = { ... }

action test() {
  bark(%{ showTeeth: true, volume: 80 })
}
```

###### Leads to Alternative Map Syntax

```
// Type:
String -> Int

// Value:
#['hello' -> 1, 'world' -> 2]
```



### Type Semantics

Shape types are *structural types*. A struct or shape type A is a **subtype** of another shape type B if A contains all properties of B (and possibly more). Names and types have to match, although given a property named p in A, p's type may be a subtype of the p defined in B.

Any **mutable** properties of a *struct* are erased from subtyping considerations. Given a shape `{ x: X }`, a struct with a single property `mut x: X` will **not** be able to subtype the shape, since `x` is mutable and thus not taken into consideration during subtyping.

###### Example

```
type Positioned2D = { x: Real, y: Real }
type Positioned3D = { x: Real, y: Real, z: Real }
// Positioned3D is a subtype of Positioned2D, but not vice versa
```



### Run-time Semantics

The **dispatch semantics** of shapes are already well defined based on the subtyping rules outlined above. However, at run-time, we have to additionally take the **run-time type** of structs and shapes into account.

A **run-time shape type** is always created with the actual property types present at run-time. This requires that each instantiation of a shape value also creates a new shape type (or finds an interned type). Such operations are costly, but this is also the only way to offer maximum flexibility for shape types. Run-time dispatch will thus always take the actual property types into account. Nominal subtyping will always be faster and may provide a performance benefit at run-time. Keep in mind, though, that this might not at all make a difference to the program you are building. Performance does not need to be chased in most cases.

**Run-time struct types** aren't quite as straight-forward. The ability to dispatch on property types effectively turns a struct type into a *family* of possible types. For each combination of actual run-time property types, a new struct type has to be created (or found if struct types are interned) if we want to support shape dispatch. This has the following drawbacks:

1. Much like with shape types, a struct type would have to be created for each instantiation of a new struct value. This is a **costly** operation, likely more expensive than creating the struct value itself. Furthermore, the more type values exist for the "same" struct type, the more type instances have to be taken into account during multiple dispatch. Non-interned but equal struct types require a costly equality check each time the dispatch cache is hit. Struct types with different run-time property types have to produce different hashes, with each different type instance taking a new place in the dispatch cache. This possibly bloats the dispatch cache of countless methods.

2. Multiple dispatch suffers from the nasty "problem" that **subtyping and type equality** are of extreme importance to generic functions. Hence, it is quite important that the usual norms of subtyping and type equality are not broken by accident. Making all struct types dependent on run-time property types without giving the programmer control over the matter is such a dangerous, implicit, accident-producing "feature".

   Take the following example. If we assume that `Coffin`'s type takes into account the run-time property type of `nail`, we have it quite trivially and on good account that the two coffins compared in the last line don't have equal types. Even worse, they are not even subtypes of one another. Since the programmer probably doesn't expect two types of the same name (in a mostly nominal typing context) to *not* be equal, this might lead to a lot of confusion. Unless the programmer specifically *states* that "yes, I want run-time struct types of the same name to vary," it's probably not a good idea to trip her up like this.

   ```
   function equalTypes(a: A, b: B): Boolean where A, B = false
   function equalTypes(a: A, b: A): Boolean where A = true
   
   trait Nail
   struct FancyNail
   struct BloodyNail
   
   struct Coffin { nail: Nail }
   
   action main() {
     let fancy = FancyNail()
     let bloody = BloodyNail()
     let coffinFancy = Coffin(fancy)
     let coffinBloody = Coffin(bloody)
     
     equalTypes(coffinFancy, coffinBloody) // --> false
   }
   ```

From the first point, it is clear that we cannot just have all properties marked as "dispatchable". There is the opportunity for different **optimizations**, but none of them are straight-forward. If we could analyze this problem perfectly, we would only mark the properties as dispatchable that are actually involved in structural dispatch. But deciding whether a run-time property type is used for run-time dispatch is impossible. It likely reduces to the halting problem. 

Taking the second point into account, assuming "open" properties not only seems detrimental to performance, but also seems like an **unsound design decision**. There are legitimate cases in which we want to have run-time dispatch on property types, but there seem to be at least as many cases in which we *don't* want to differentiate between run-time property types.

So in the end, the best option seems to be to make run-time variance of property types **explicit**. One must thus declare a *struct* property as `open` if it should be able to partake in run-time structural type dispatch. If a property is not marked as `open`, its type as known by the struct type is always the property's compile-time type.

```
struct Position3D implements Position { x: Real, y: Real, z: Real }

struct Hero {  
  health: Health
  open position: Position
}

type +Position = { position: Position }
type +Position3D = { position: Position3D }

// TODO: If we only mark properties as selectively open, we cannot allow shape types to be abstract.
action move(entity: +Position) { ... }
action move(entity: +Position3D) { ... }

action test() {
  let hero = Hero(Health(), Position3D(0, 0, 0))
  
  // Dispatches to the second move function, since position is an open property.
  // If position wasn't open, the call would dispatch to the first move function.
  move(hero)
}
```



### Example: Component-based Programming

...


