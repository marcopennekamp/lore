# Typed Components

One of our main goals with Lore is to create a language that brings static typing to entity component systems commonly used in games. However, we believe that the fundamental concept of composition is worth investigating, both in contrast to and in cooperation with more traditional OOP approaches.

To achieve that, we will keep the language as general as possible, so that it may be used in contexts apart from game development.




## The Problem with Untyped Components

In notable entity-component systems (like Unity or Defold), components are added to an entity at runtime. Both the engine editor and the compiler of the scripting language thus can't guarantee that an entity has a specific component. Entities may be added to collections that provide easy access to specific components, for example when a renderer wants to render the list of all character-like entities. There is no compile-time guarantee that an entity actually has the required component. Or, in the case that components themselves are in these lists, that the component is still part of the entity. 














## Components

### The has-relationship


#### Transitivity of the has-relationship

Components are regular types that may be included as components in any other type. This allows the programmer to create a component hierarchy, which poses the following question to the language designer: **Are components of components first-class components?**

In other words, suppose we have the following type:

    Part has P1 has P2
    
Here, **P1** is a component of **Part**, as is **P2**.

Now we have the following type:

    A has Part
    
The question is, does **A** have the type `A has Part` or the type `A has Part has P1 has P2` by transitivity? This has practical implications: 

1. If a function requires a type with a particular component, whether the has-relationship is transitive may decide if the function can be applied to the type. 
2. Similarly, the `:`-Operator will behave differently depending on the transitivity of **has**. A strong argument against transitivity is that, with more complex component hierarchies, the transitive closure of a type becomes increasingly complex, to a point where accessing the entire namespace of functions and attributes with the `:`-Operator will either be ambiguous too often or simply too much to handle for a programmer's mind. 

We can also construct an example to show that transitivity is perhaps not the best idea. Suppose we have a 

At this point, we should probably dismiss the idea of a transitive has-relationship. But we are always open to discussion, of course.



### The `:`-Operator



### Component Dependencies

A component should be able to require access to other components of the object it is attached to.

    record Sprite requires Position {
      mut image: Image
    }
    
    
### Adding and Removing Typed Components

While adding components to and removing components from an entity can be achieved even with typed components, we can't update existing entity types at runtime to compensate for an added or removed component. However, behaviour such as this is exactly what we want to disallow with typed components **most of the time**. We don't want to accidentally pass an entity that does not have a Sprite to the `render(entity: Any has Position has Sprite)` function. If we use this code with our entity, surely we should not be able to remove the Sprite component.

However, in some cases it is beneficial to have a more dynamic entity type. Maybe some entity only really has some component in certain cases. In that case, we also probably want functions that are only executed if the specific component is currently part of the entity. 

Solution: dynamic entity types, monads / syntactic support for runtime component resolution




