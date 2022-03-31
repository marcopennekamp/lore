## TODO

#### Types

- Map types: Covariance and contravariance?
- Naming of declared types: The name "Declared Types" clashes with the fact that type aliases can also be "declared", globally, in a module, and so on. Maybe we should call these types "Data Types" or something else.
- Shouldn't an intersection type only be abstract if ALL its parts are abstract? Wouldn't that mean that we can throw away the whole concept of augmentations? Are augmentations just a cover-up for the shoddily defined abstractness of intersection types?
  - Given a type `A & B` with `A` being concrete. Let's say we have values `v: A & B`, which must thus also be type-able as `v: A`. If `A` is concrete, a value of `A & B` should either not be constructable, or there MUST be a `v` that is a concrete value. The problem is, I think, constructing a type `A & B` where `A` is concrete without `B` being trivially subsumed by `A`, making the type actually `A`.
  - We also need augmentations at least for shapes. Because if a shape type stands alone, it's definitely concrete, but in combination with a trait in an intersection type, it's abstract.
  - We might want to reanalyze abstract and concrete types from a "value count" perspective. That is, how many values would inhabit the type? Because from that perspective, the intersection of two types would basically multiply the value counts of each individual type. So from that angle, the current definition of one type needing to be abstract is correct. 


#### Multi-functions


#### Traits and Structs


#### Shapes


#### Expressions
