## TODO

#### Types

- Map types: Covariance and contravariance?
- Naming of declared types: The name "Declared Types" clashes with the fact that type aliases can also be "declared", globally, in a module, and so on. Maybe we should call these types "Data Types" or something else.
- Shouldn't an intersection type only be abstract if ALL its parts are abstract? Wouldn't that mean that we can throw away the whole concept of augmentations? Are augmentations just a cover-up for the shoddily defined abstractness of intersection types?
  - Given a type `A & B` with `A` being concrete. Let's say we have values `v: A & B`, which must thus also be type-able as `v: A`. If `A` is concrete, a value of `A & B` should either not be constructable, or there MUST be a `v` that is a concrete value. The problem is, I think, constructing a type `A & B` where `A` is concrete without `B` being trivially subsumed by `A`, making the type actually `A`.
  - We also need augmentations at least for shapes. Because if a shape type stands alone, it's definitely concrete, but in combination with a trait in an intersection type, it's abstract.
  - We might want to reanalyze abstract and concrete types from a "value count" perspective. That is, how many values would inhabit the type? Because from that perspective, the intersection of two types would basically multiply the value counts of each individual type. So from that angle, the current definition of one type needing to be abstract is correct. 


#### Multi-functions

- Expand the "Practical Uses" section with the following topics:
  - Behavioral and structural abstraction with traits
  - Multiple dispatch with intersection types (including component-based programming)
  - Extension methods (example: adding functions to Pyramid)
- **Multi-Calls:** A multi-function call that, Instead of calling exactly one function, calls *all* candidates in the min set.
  - Alternative: Introduce a `min` function that returns a list of all functions that *would* be called. Then the user can decide to call them right away or work with them in some other way.


#### Traits and Structs

- Dynamic specialization and generalization (attaching and removing trait types at run time) could be replaced with predicate dispatch. We'll have to analyze the performance impact and practicality of both approaches.
- **Map syntax alternative:**
  ```
  %{ name: 'Mellow', position } as Person
  Person(%{ name: 'Mellow', position })  // This clashes with the idea that Person is a unique function value.
  %Person{ name: 'Mellow', position }
  ```

  This allows us to work with the established shape syntax instead of having two parallel syntaxes.
- Easy **getters and setters** for struct properties?
- **Syntactic Sugar for Properties:**
  ```
  trait Position
  property x: Real of Position
  
  act test(pos: Position)
    println(pos.x)
  end
  ```
  
  This is internally still a multi-function definition. Here is the general syntax:

  ```
  property name: Type of Trait
  --> func name(self: Trait): Type
  
  property mut name: Type of Trait
  --> func name(self: Trait): Type
  --> func set_name(self: Trait, value: Type): ()
  ```

  And then implement it like this:

  ```
  // Direct mapping
  struct Point extends Position
    x: Real implements Position.x
  end
  
  // Indirect mapping
  property x: Real of box: Box = box.x_start + width(box) / 2
  ```

  This could also be used as syntactic sugar for derived properties:

  ```
  property width: Real of box: Box = box.x_end - box.x_start
  property x: Real of box: Box = box.x_start + box.width / 2
  ```

  The `property` syntax thus wouldn't only be allowed for traits but for any types. We could define properties over tuples:

  ```
  property first: A of tuple: (A, B) where A, B = get(tuple, 0)
  ```
  - **Alternative:**

    ```
    trait Position
      x: Real
    end
    
    act test(pos: Position)
      println(pos.x)
    end
    
    // Direct mapping
    struct Point extends Position
      x: Real implements Position.x
    end
    
    // Indirect mapping
    property x: Real of box: Box = box.x_start + width(box) / 2
    // or:
    func x(box: Box): Real = box.x_start + width(box) / 2
    // or:
    property x(box: Box): Real = box.x_start + width(box) / 2
    ```

    This would internally still be represented by multi-functions, but the declaration in traits is shorter and more natural. **Potential downside:** This way of declaring trait properties could confuse users into thinking that properties are inherited. It could also make the idea that trait properties are just multi-functions under the hood harder to convey. Another question is which namespace/module these property functions will be part of.

    Another downside is that we can't easily tie properties to ANY types with this syntax, which would be especially problematic for computed properties of structs. However, we could consider supporting both syntaxes. In fact, the trait property syntax would be the natural way for traits (at the trait's declaration site, of course, not for "monkey patching"), while the `property` syntax would be the natural way for other types.
- One step further: Automatic, optional **memoization of properties**.
- The lack of struct inheritance currently has the big disadvantage that one cannot **"mix in" property definitions**. If you have a trait `Entity` that requires all its implementors to specify `name: String` and `sprite: Sprite` properties, this has to be re-declared inside every struct that extends `Entity`. I can see two main ways to solve this problem: (1) add mixins as a language feature or (2) allow users to solve this problem with a macro system. **Mixins** would firmly concern themselves with the realm of data representation; they would not even define their own types. Hence, adding mixins would preserve our stated goal of separating data representation and abstract data structure and behavior. You could declare a mixin alongside a trait if close data coupling is desired, but each struct would at least have to declare that it's using the mixin. There would be no language-level coupling between mixins and traits.
  - A way to implement mixins would be **mixing in shape types**.

    ```
    struct Position
      mut x: Real, mut y: Real
    end
    
    type +Position = %{ position: Position }
    
    struct Player
      mix +Position
    end
    ```
    
    However, this does not support mixing in mutable properties, and thus couldn't be the only solution.
- **Ad-hoc envelope types:** Lore will support envelope types. To make "type all the things!" particularly easy, Lore should allow you to create ad-hoc open envelope types when defining structs:

  ```
  struct Position
    x: Real as X
    y: Real as Y
    z: Real as Z
  end
  ```

  Each envelope type becomes part of the (companion) module of the struct, so the code above implicitly declares the following:

  ```
  module Position
    envelope X(Real)
    envelope Y(Real)
    envelope Z(Real)
  end
  ```

  However, the ad-hoc definition has the additional advantage that **envelope types are constructed internally**. Take the following example:

  ```
  struct Account
    id: Int as Id
    name: String as Name
    score: Real as Score
  end
  
  let jeremy = Account(1, "Jeremy", 15.37)
  > jeremy.id : Account.Id
  > jeremy.name : Account.Name
  > jeremy.score : Account.Score
  ```

  The constructor takes the underlying values as arguments and doesn't require any envelope boilerplate.


#### Expressions

- **Default string order** could be improved by revising the lexicographic comparison algorithm. Comparing by code points is valid, but doesn't take the nature of the characters into account.
  - It would be even better if we could take locale into account, but that's out of place for a *default* order and fits more with a library function that sorts strings based on locale.
- **Struct/shape order:** Should we define a default order for `struct < shape` comparisons?
- Currently, we have type ascriptions, which cannot convert values to other values, and type conversions from int to real and real to int. We are missing **declared type narrowing**, i.e. an operator that allows casting a declared type to a narrower type, such as `Animal` to `Fox`. This would be accompanied by a run-time check.
  - We can implement this as a function, as it mirrors the "type filter" approach, but the multi-function would need to take a concrete type argument. This is only possible via the "struct hack" also employed by `type-filer.lore`. Instead, we can first implement static functions without dispatch, which is already a proposal, and then use those to easily implement a narrowing cast.
- Consider introducing **Swift-style `guard` statements** with a twist: They operate within blocks. If the condition is false, continue the code, otherwise *return the value of the else part from the block*.
- A feature such as Swift's **trailing closures**.
- **If-else evaluating to Option:** If only the truthy branch of an if-else expression is defined, return an `Option`, with the implicit `else` returning `None`.


#### Modules

- **Use anywhere:** The `use` declaration should be usable anywhere (inside expressions as a top-level expression) for more fine-grained control of names.
  - To support this, we can implement imports in *local scopes* as well.
- **Aliases:** The `use` declaration should allow the programmer to rename a given symbol.
- **Complex imports:** Allow imports such as `use lore.[Enum._, Tuple]` and `use lore.[Enum.[flat_map, map], Tuple]`.
- **Absolute declarations:** With the syntax `func .lore.core.to_string` (and the equivalent for other top-level declarations), a user can declare a function with an absolute path, which would disregard its surrounding module declarations. This would allow us to override core functions without the need for specifying a new module. Especially so because currently, wanting to override core/Enum/... functions basically disallows using a top module declaration. For many modules, this either necessitates defining core/Enum/... functions in a separate file, or surrounding all definitions in the file with a module declaration, even the "main" definitions (such as `lore.List` definitions in `pyramid/list.lore`). The latter approach introduces needless indentation, while the former approach fragments a logical unit into multiple files.
  - The same should be possible for modules, e.g. `module .lore.core do ... end`.


#### Specs

- The biggest weakness of the current specs is the inability to define private global test data and test helpers. This is not an issue for tests like in the `test/` folder that are compiled independently, but for specs such as those compiled with Pyramid, for example, we cannot pollute the `lore._` namespace with test globals and functions. This needs to be remedied in one way or another. (e.g. private modules, spec-only globals and functions, or some other solution.)
- Idea: Provide a module `lore.bench` with a global variable `lore.bench.bucket`. `bucket` is a struct with a single field `value: Any` that can be used to let a result escape from the benchmarking function, thereby negating compiler optimizations. This is currently not useful as unused expressions aren't optimized away, but might be in the future.
  - An obvious alternative to this is simply not performing these optimizations inside of specs. This might be the preferable solution, but might also be too tricky to implement, or come with unacceptable edge cases that I currently don't anticipate.
