# Shapes

A **shape** is a partial view on structured data, with the option for ad-hoc representation. A shape consists of a set of **immutable properties**, much like a struct. However, a shape does not define a constructor of any kind and, crucially, has **no name**. If properties agree, a shape type can describe a struct, making the struct a **subtype** of the shape. **Shape values** can also be created in place, without specifying a type. Such an ad-hoc representation is similar (especially in convenience) to maps or objects in dynamically typed languages.

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

Shape types are *structural types*. A struct or shape type A is a **subtype** of another shape type B if A contains all properties of B (and possibly more). Names and types have to match, although given a property p in A, p's type may be a subtype of the p defined in B.

Any **mutable** properties of a *struct* are erased from subtyping considerations. Given a shape `{ x: X }`, a struct with a single property `mut x: X` will **not** be able to subtype the shape, since `x` is mutable and thus not taken into consideration during subtyping.

###### Example 1

```
struct Position3D implements Position { x: Real, y: Real, z: Real }

struct Hero {
  position: Position3D
  health: Health
}

type +Position = { position: Position }
// Hero is a subtype of +Position, given Position3D <: Position
```

###### Example 2

```
type Positioned2D = { x: Real, y: Real }
type Positioned3D = { x: Real, y: Real, z: Real }
// Positioned3D is a subtype of Positioned2D, but not vice versa
```



### Dispatch Semantics

Since **dispatch semantics** are derived from subtyping, it is at this point well defined how shape types affect multiple dispatch. 











Component-Based Programming





### Run-time Considerations

**Shape values are not cheap to type-check**, especially at **run-time**, which affects dispatch performance. Nominal subtyping will always be faster and may provide a performance benefit at run-time. Keep in mind, though, that this might not at all make a difference to the program you are building. Performance does not need to be chased in most cases.

**Run-time struct types** would be quite straight-forward without shape types. For each struct type, at run-time, there would be exactly one type instance. The ability to dispatch on property types via shape types turns a struct type into a *family* of possible types, since each different combination of property types is its own type. Even worse, we cannot decide in the general case which property types a given struct can contain, because property types depend on the execution of the program (and possibly on user input).

This essentially leads to the drawbacks presented in the next two sections.

##### Instantiation

During **struct instantiation**, a struct type will have to be created (and hashed) that incorporates the individual property types. The vast number of properties will not be used for structural dispatch, but some will. It is a challenge to identify these few in order to optimize the general case for the many. Without any optimization, a new type object must be created for each new struct instance. Alternatively, if struct types are interned, such a type object may be *found*. But in either case, each instantiation will incur additional performance penalties, which only grow the more properties a struct has.

As we anticipate that instantiation will be one of the most "popular" operations, we should take care to optimize this as best as possible.

**Optimizations:**

- **Implicit:** If a property isn't part of any shape type declared anywhere in the program (by name, definitely, and perhaps also when analyzing the specific type), we can safely exclude that property from the run-time type. This is a form of whole-program optimization.

  - Drawback: If we just decide by name, we will effectively have made the "global namespace" problem already inherent in property names even worse. This seems to be fine for small programs, but the larger the program gets, the higher the chance that a given name is used for structural dispatch. This would lead to performance regression for large programs (which are especially harder to benchmark). It seems inevitable that, should we go this route, we will have to incorporate type analysis into the optimization.

  - Note that we can limit the analysis to shape types included as parameter types of multi-functions.

  - It suffices to look at individual shape types, since there is no way to "remove" a property from a shape type. If we look at all shape types to decide whether a struct could possibly be a subtype of any of the given shape types, and we get the answer "no", the struct can't suddenly be a subtype of the *intersection* of two of these shape types.

  - It does not suffice to look at individual shape types. For example, a more complex shape type may be built by intersecting multiple shape types.

    - It may suffice after all. Since there is no way to "remove" a property from a shape type, a more complex type can only add properties. So 

  - Type parameters may make this kind of analysis impossible or at least very hard. In the example below, a struct `Bar` has to dispatch to either the first or second `foo` depending on the type of `x`. The type variable adds enough indirection to possibly trip up the algorithm.

    ```
    trait X
    trait Y extends X
    struct S1 implements X
    struct S2 implements Y
    
    action foo(options: Any) = ...
    action foo(options: { x: A }) where A <: Y = ...
    
    struct Bar { x: X }
    
    action main() {
      foo(Bar(S1())) // invokes the first foo, since S1 is not a subtype of Y
      foo(Bar(S2())) // should invoke the second foo
    }
    ```

    Thinking about it further, type parameters indeed make the analysis impossible: A type parameter moves type calculations to the runtime. Hence, we cannot in all cases decide whether a given struct could be a subtype of any shape type, because some of these subtyping queries only become evident at run-time.

    The nail in the coffin:

    ```
    function subtype(a: Any, b: Any): Boolean = false
    function subtype(a: A, b: B): Boolean where A, B <: A = true
    
    trait Nail
    struct FancyNail
    struct BloodyNail
    
    struct Coffin { nail: Nail }
    
    action main() {
      let fancy = FancyNail()
      let bloody = BloodyNail()
      let nailedFancy = %{ nail: fancy }
      let nailedBloody = %{ nail: bloody }
      let coffinFancy = Coffin(fancy)
      let coffinBloody = Coffin(bloody)
      
      subtype(nailedFancy, nailedBloody) // --> false
    }
    ```

    

- **Explicit:** Struct properties have to be declared as `open` to be considered for run-time dispatch. If not, their compile-time type will be considered.

  - This is a potential pitfall as it makes shape dispatch more opaque. Instead of being able to dispatch on all immutable properties, the user of a struct has to know whether a given property is open or not. This is an additional source of errors.
  - This solution is essentially similar to how components are treated now. Since only components are relevant for subtyping at run-time, we have scoped the solution correctly. Taking into account all properties would have been overkill.
  - From a subtyping perspective, `open` should be explicit because otherwise a programmer would have no control over the type equality of structs. The question of whether a given property's type should determine the run-time type of a struct must not always be answered with yes. Just as there are cases where taking property types into account with the larger struct type is beneficial, there are other cases where this would be seriously detrimental. For an example, look at the "nail in the coffin" code above.









### 














### Structural Type Performance

What about the performance of checking structural typing at run-time? Would this be a major bottleneck?
Structural types probably need to be interned at least.

