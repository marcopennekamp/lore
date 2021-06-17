# Structs, Traits, and Shapes

**Structs, traits and shapes** are the abstractions with which complex data types can be built in Lore. These three concepts can be roughly differentiated as such:

- **Structs** are data type definitions that can stand on their own, possibly extending some number of traits. A struct is always a subtype of all the traits that it extends and also carries this information at run-time. However, unless a trait is attached at run-time (which will be possible under certain circumstances), the traits that are attached to the instance of a struct are determined at compile-time.
- **Traits** are abstractions of structure and behavior. They are Lore's solution to the questions of data type abstraction, inheritance, and interfaces. Traits cannot be instantiated directly: they have to be backed by a struct.
- **Shapes** describe partial data structures. Shape types define a set of immutable properties which a struct must have (fitting in name and type) to be considered a subtype of the shape type. Apart from structs being viewed through the lens of shapes, Lore also offers shape values, which are unnamed, ad-hoc instances of a shape type. Shapes offer the power of structural typing at compile-time *and* run-time. They are an excellent abstraction for implementing entity-component systems.



### Structs

A **struct** describes the type and representation of user-defined data. In concrete terms, a struct is a set of **properties** (immutable and, if you must, mutable) allowing optional default values. A struct can **extend** any number of traits, which together determine the functions that must be implemented for the struct and, together with shape types, the properties a struct must contain. An instance of a struct, also called a struct value or simply struct, can be **constructed** using one of two provided syntaxes. As structs describe actual instances, a struct type is **always concrete and never abstract**.

Structs do not support **inheritance** in and of themselves. Traits are the mechanism to facilitate inheritance, which has the huge advantage of cleanly separating the concerns of data representation (structs) and abstract structure/behavior (traits, shapes).

Properties can be **delimited** using commas and newlines. Both styles are permitted interchangeably, giving the ability to use both in the same struct definition, and should be chosen based on readability. Properties can be accessed using the dot notation `struct.property`.

###### Syntax Example

```
struct Point { x: Real = 0, y: Real = 0, z: Real = 0 }
struct Position { mut point: Point }
struct Person {
  name: String
  age: Int = 20
  calling: String = 'Arts and Science'
  position: Position
}
```

##### Construction

Creating new struct instances is possible with two independent **constructor** syntaxes. The **call syntax** is a convenient way to create instances, but with the requirement that all properties need to be specified, including those that could have a default value. In contrast, the **map syntax** is most convenient when default values should be applied to properties or when more self-evident code is desired. It is also useful when properties should be specified out of order.

When using the map syntax, one may **omit some verbosity** in a definition like `property = value` if the value is a variable and named exactly like the property. An example of this is included below.

Apart from these two constructor styles, all **derivative constructors** will have to be defined as ordinary (multi-)functions. 

###### Syntax Example

```
action test() {
  // Call syntax.
  let point = Point(0.5, 1.5, 2.5)
  let position = Position(point)
  // Map syntax.
  let person = Person { name = 'Mellow', Position = position }
}

action test2() { 
  // If the variable name matches the property name, it's possible to omit the colon entirely.
  let name = 'Shallow'
  let person = Person { name, Position = Position(Point { }) }
}
```

##### Extending Traits

A struct can **extend** any number of traits. This will make the struct a **subtype** of each of its traits. Since a struct is always a concrete type, **all abstract functions** of the traits will have to be implemented for the struct. This is implicitly handled by the constraints governing abstract functions and doesn't need to be handled specially for structs.

A struct can also extend **shape types**. This will require the struct to declare properties in such a way that the struct subtypes the given shape type(s).

###### Syntax Example

```
trait Hashable
function hash(hashable: Hashable): Int

struct Person extends Hashable { name: String }
function hash(person: Person): Int = /* Compute the hash... */
```

##### Open Properties

Normally, the run-time type of a struct's property is not part of the run-time type of the struct. The struct will simply assume its compile-time type at the point of construction. However, some properties may need to be typed at run-time to support structural subtyping. Such properties must be declared to be **open**. You can read more on the reasoning behind this down in the section about shapes.

###### Syntax Example

```
struct Soldier {
  open weapon: Weapon
}
```



### Traits

A **trait** is a type that describes structure and behavior. In concrete terms, a trait is an abstract type that can be associated with functions defining both its **structure** and **behavior** (either abstract, concrete, or both). Since traits must be backed by a struct, meaning the trait itself cannot be instantiated, a trait is **abstract**.

Traits can also **inherit** from (multiple) other traits and even extend shape types. A trait `A` inheriting from a trait `B` will make `A` a strict and direct subtype of `B` and give the programmer the opportunity to specialize any functions declared over `B` for the type `A`. When extending a shape type `S`, the trait effectively declares that any struct extending the trait must contain all properties declared in `S`.

###### Syntax Example

```
trait T extends A, B, S
```

##### Traits and Structure

Using a trait to create **data abstractions** is as simple as defining the right (abstract) functions. This is not a special language feature, because we are simply using existing features such as multi-functions and multiple dispatch, so an example will suffice. We could define a trait `Position` that declares the following abstract functions:

```
trait Position
function x(pos: Position): Real
function y(pos: Position): Real
function z(pos: Position): Real
```

This allows us to define various structs extending the same `Position`, for example:

```
struct Point extends Position { x: Real, y: Real, z: Real }
struct Box extends Position { 
  xStart: Real, xEnd: Real
  yStart: Real, yEnd: Real
  zStart: Real, zEnd: Real
}
```

Of course, we also have to implement the abstract functions declared by `Position` for each of the structs extending the trait:

```
function x(point: Point): Real = point.x
function y(point: Point): Real = point.y
function z(point: Point): Real = point.z

// The position of a box is its center!
function x(box: Box): Real = box.xStart + width(box) / 2
function y(box: Box): Real = box.yStart + height(box) / 2
function z(box: Box): Real = box.zStart + depth(box) / 2
```

You might think this will become untenable with a global namespace. You would be mostly right, although multiple dispatch still does a lot of heavy lifting even in the disambiguation department. We will introduce namespacing, likely in the form of modules, as an update to the language shortly (in terms of update order, not necessarily time) after the minimum viable language has been completed. Properly namespaced properties are a big motivator for us to introduce a module system early.

Finally, we could declare a function that just works with the data provided by `Position`: 

```
function distance(pos1: Position, pos2: Position): Real = {
  let dx = x(pos2) - x(pos1)
  let dy = y(pos2) - y(pos1)
  let dz = z(pos2) - z(pos1)
  sqrt(dx * dx + dy * dy + dz * dz)
}

action test() {
  let box = Box(0, 10, 0, 10, 0, 10)
  let point = Point(3, 7, 9)
  println(distance(box, point)) // 3.4641...
}
```

Adding inheritance to this example would, perhaps, allow us to model positions of different dimensions:

```
trait Position2D
function x(pos: Position2D): Real
function y(pos: Position2D): Real

trait Position3D extends Position2D
function z(pos: Position3D): Real
```

Any struct extending `Position3D` will have to provide a definition for all three of these abstract functions.

In the future, we might introduce **syntactic sugar** for the simpler forms of data abstraction, especially so that implementing data-heavy traits with a struct isn't ultra tedious. For now, we want to keep it simple though, and the idea of multi-functions once again proves to be powerful enough to get there.

##### Traits and Behavior

Traits are natural abstractions for **behavior**. Just as with data abstraction, multiple dispatch provides the ability to work with abstract and concrete functions. To Lore, a trait itself is just an "empty" type. All the magic happens within the multi-functions.

As a simple **example** of behavior abstractions, consider a trait `Hashable` that requires its implementors to provide a `hash` function:

```
trait Hashable
function hash(value: Hashable): Int
```

Consider we have a trait `Statistic` that should be hashable, so that we can use stats as keys in a map:

```
trait Statistic extends Hashable
function uniqueName(stat: Statistic): String
```

Instead of implementing the hash function for every stat individually, we can implement it just for the trait, relying on the unique name supplied by other stats:

```
// Assuming that hash is implemented for Strings...
function hash(stat: Statistic): Int = hash(uniqueName(stat))
```

Note that `Statistic` wouldn't need to extend the `Hashable` trait just to provide an implementation for the `hash` function. The value of having `Statistic` implement `Hashable` is chiefly twofold:

1. We can be sure that all functions required by `Hashable` are implemented for `Statistic`.
2. We can treat a `Statistic` as a `Hashable`.

##### Traits as Label Types

Conceptually, a **label type** is a type that adds some additional semantic meaning to another type it's attached to. For example, a sorted list could be represented as `[A] & Sorted` and a dead monster could be represented as `Monster & Dead`. We would declare these label types as follows:

```
trait Sorted
trait Dead
```

The core usefulness of a label type comes from the idea that we can **specialize functions** when the label is present:

```
action hit(monster: Monster) { ... }
action hit(monster: Monster & Dead) {
  // Do something else if the monster is dead.
}
```

Right now, it is not possible to attach a label type to a value at run-time, so label types can only be "attached" when a struct is instantiated. But once we introduce **dynamic specialization and generalization**, label types will be attachable to and removable from existing values, provided their compile-time types still agree. Then it becomes a matter of moving labels traditionally handled as object properties to the type space and harnessing the power of multiple dispatch. For example, one could attach their own label type to values that are declared in a library, then specialize some library functions for types that also have the label.



### Shapes

A **shape** is a partial view on structured data, with the option for ad-hoc representation. A shape consists of a set of **immutable properties**, much like a struct. However, a shape does not define a constructor of any kind and, crucially, has **no name**. If properties agree, a shape type can describe a struct, making the struct a **subtype** of the shape. **Shape values** can also be created in place, without specifying a type. Such an ad-hoc representation is similar (especially in convenience) to maps or objects from dynamically typed languages.

At run-time, multi-function calls are **dispatched** on the actual property types evidenced by the struct or shape value. This allows a Lore programmer to specialize functions based on properties, effectively enabling styles such as **component-based programming**.

###### Syntax Example

```
type Positioned = { x: Real, y: Real, z: Real }
type Dimensioned = { width: Real, height: Real, depth: Real }
type Spaced = Positioned & Dimensioned
```


##### Shape Values

Shapes can be directly constructed as **values**. This comes in handy for ad-hoc data structures, such as multi-part values returned from a function or options passed into a function or constructor. Such values are **not** structs. All properties of a shape value are immutable.

###### Syntax Example

```
type Options = { showTeeth: Boolean, volume: Real  }

function bark(options: Options): String = { ... }

action test() {
  bark(%{ showTeeth: true, volume: 80 })
}
```

##### Type Semantics

Shape types are *structural types*. A struct or shape type A is a **subtype** of another shape type B if A contains all properties of B (and possibly more). Names and types have to match, although given a property named p in A, p's type may be a subtype of the p defined in B.

Any **mutable** properties of a *struct* are erased from subtyping considerations. Given a shape `{ x: X }`, a struct with a single property `mut x: X` will **not** be able to subtype the shape, since `x` is mutable and thus not taken into consideration during subtyping.

**TODO:** We might want to rethink the mutability restriction. Mutable properties would *only* need to require type mutation if the property in question is also open. Otherwise, the type need not change on reassignment since the run-time type of the property is of no concern.

###### Example

```
type Positioned2D = { x: Real, y: Real }
type Positioned3D = { x: Real, y: Real, z: Real }
// Positioned3D is a subtype of Positioned2D, but not vice versa
```

##### Run-time Semantics

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
struct Position3D extends Position { x: Real, y: Real, z: Real }

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

##### Component-based Programming

One way to build programs with a healthy level of abstraction is **component-based programming**. The idea is that data consists of multiple components which can each provide already implemented functionality. For example, an **entity** called `Hero` may have a component `Position` and a component `Shape`. There may be functions such as `move` to manipulate position and a function `render` that requires both a `Position` and a `Shape` to work.

We can support such **entity-component systems** natively (and especially with type safety) in Lore using structural typing. The following code implements the constellation of Hero, Position and Shape:

```
struct Position { mut x: Real, mut y: Real, mut z: Real }
type +Position = { position: Position }

action move(entity: +Position, distance: Real, direction: Vector3) {
  // calculate new x, y and z coordinates...
  entity.position.x = x
  entity.position.y = y
  entity.position.z = z
}

struct Shape { width: Real, height: Real, depth: Real, model: Model }
type +Shape = { shape: Shape }

action render(entity: +Position & +Shape) {
  // use entity.position and entity.shape to render the entity...
}

// The `extends` is not strictly necessary, but provides additional compile-time safety should either
// +Position/+Shape or the corresponding properties change unexpectedly.
struct Hero extends +Position, +Shape {
  position: Position  // Note that position does not need to be open since structs can't have subtypes.
  shape: Shape
}
```

The remarkable benefit of this approach: any struct or shape that contains a property `position: Position` and `shape: Shape` can be used as an entity for the  `move` and `render` functions. We can conceive any number of additional entities that can just as much use these already existing functions. This enables **generic programming over partial structures**, i.e. entities and components.

**Specialization** is another great aspect of this programming model. Imagine we want to implement an additional `render` function for those entities that not only have `Position` and `Shape`, but also `Color`. We can simply write a second function:

```
action render(entity: +Position & +Shape & +Color) {
  // use position, shape AND color to render the entity...
}
```



### Post-MVL Extensions (Ideas)

- Easy **getters and setters** for struct properties?

- **Visibility declarations** like private/public/protected for struct properties?

  - It'd probably be best to keep visibility in the module system. I don't see many advantages in building firewalls for data definitions. If a struct desperately needs to hide some data, it should hide behind a trait abstraction.

- **Syntactic Sugar for Properties:**

  ```
  trait Position
  property x: Real of Position
  
  action test(pos: Position) {
    println(pos.x)
  }
  ```

  This is internally still a multi-function definition. Here is the general syntax:

  ```
  property name: Type of Trait
  --> function name(self: Trait): Type
  
  property mut name: Type of Trait
  --> function name(self: Trait): Type
  --> function setName(self: Trait, value: Type): ()
  ```

  And then implement it like this:

  ```
  // Direct mapping
  struct Point extends Position {
    x: Real extends Position.x
  }
  
  // Indirect mapping
  property x: Real of box: Box = box.xStart + width(box) / 2
  ```

  This could also be used as syntactic sugar for derived properties:

  ```
  property width: Real of box: Box = box.xEnd - box.xStart
  property x: Real of box: Box = box.xStart + box.width / 2
  ```

  The `property` syntax thus wouldn't only be allowed for traits but for any types. We could define properties over tuples:

  ```
  property first: A of tuple: (A, B) where A, B = get(tuple, 0)
  ```

  Note: Since this proposal does not add any additional expressiveness to the language (only convenience), it is not a candidate for the MVL itself. Also, another question is how this system interacts with namespacing, and thus it would be prudent to define the module system first and THEN turn our attention to this proposal.

  - **Alternative:**

    ```
    trait Position {
      x: Real
    }
    
    action test(pos: Position) {
      println(pos.x)
    }
    
    // Direct mapping
    struct Point extends Position {
      x: Real extends Position.x
    }
    
    // Indirect mapping
    property x: Real of box: Box = box.xStart + width(box) / 2
    // or:
    function x(box: Box): Real = box.xStart + width(box) / 2
    // or:
    property x(box: Box): Real = box.xStart + width(box) / 2
    ```

    This would internally still be represented by multi-functions, but the declaration in traits is shorter and more natural. **Potential downside:** This way of declaring trait properties could confuse users into thinking that properties are inherited. It could also make the idea that trait properties are just multi-functions under the hood harder to convey. Another question is which namespace/module these property functions will be part of.

    Another downside is that we can't easily tie properties to ANY types with this syntax, which would be especially problematic for computed properties of structs. However, we could consider supporting both syntaxes. In fact, the trait property syntax would be the natural way for traits (at the trait's declaration site, of course, not for "monkey patching"), while the `property` syntax would be the natural way for other types. Going a step further: `property` would be the keyword for functions that accept a single `instance` parameter and are called like `instance.property` *without* parentheses. (What about setters, then?)

- One step further: Automatic, optional **memoization of properties**.

- The lack of struct inheritance currently has the big disadvantage that one cannot **"mix in" property definitions**. If you have a trait `Entity` that requires all its implementors to specify `name: String` and `sprite: Sprite` properties, this has to be re-declared inside every struct that extends `Entity`. I can see two main ways to solve this problem: (1) add mixins as a language feature or (2) allow users to solve this problem with a macro system. **Mixins** would firmly concern themselves with the realm of data representation; they would not even define their own types. Hence, adding mixins would preserve our stated goal of separating data representation and abstract data structure and behavior. You could declare a mixin alongside a trait if close data coupling is desired, but each struct would at least have to declare that it's using the mixin. There would be no language-level coupling between mixins and traits.

  - A way to implement mixins would be **mixing in shape types**.

    ```
    struct Position { mut x: Real, mut y: Real }
    type +Position = { position: Position }
    
    struct Player {
      mix +Position
    }
    ```

- **Companion namespaces** for any declared type. (See also the next proposal in this list.)

- **Ad-hoc envelope types:** Lore will support envelope types. To make "type all the things!" particularly easy, Lore allows you to **create ad-hoc open envelope types when defining structs:**

  ```
  struct Position {
    x: Real as X
    y: Real as Y
    z: Real as Z
  }
  ```

  Each envelope type becomes part of the (companion) namespace of the struct , so the code above implicitly declares the following:

  ```
  namespace Position {
    envelope X(Real)
    envelope Y(Real)
    envelope Z(Real)
  }
  ```

  However, the ad-hoc definition has the additional advantage that **envelope types are constructed internally**. Take the following example:

  ```
  struct Account {
    id: Int as Id
    name: String as Name
    score: Real as Score
  }
  let jeremy = Account(1, "Jeremy", 15.37)
  > jeremy.id : Account.Id
  > jeremy.name : Account.Name
  > jeremy.score : Account.Score
  ```

  The constructor takes the underlying values as arguments and doesn't require any envelope boilerplate.
  
- **Attaching properties at run-time:** Adding properties to arbitrary structs and shapes could be very powerful combined with structural dispatch. (Especially to dynamically add components to a struct.)

