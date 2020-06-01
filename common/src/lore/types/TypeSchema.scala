package lore.types

/**
  * A type schema is a sort of type constructor. It expects a list of type parameters to instance a
  * concrete (monomorphic) type.
  *
  * @tparam A The kind of monomorphic type this schema will produce.
  */
trait TypeSchema[+A <: Type] {
  /**
    * All type parameters of the type schema in order.
    */
  def typeParameters: List[TypeVariable]

  /**
    * Instantiates the type schema with the given type arguments. If you are unsure whether this operation
    * will go smoothly, call canInstantiate.
    */
  // TODO: When we implement the runtime version of this, we'll have to think about caching. It's not only that
  //       every time we create the instance of a declared type, we'd have to create a new instance of the declared
  //       type, we'd also have to create an instance of the supertype and its supertype and so on. Either we will
  //       have to cache the type globally (in a sort of name/argument types structure), or at least cache it
  //       "locally" so that we don't create a new instance of the schema every time an object is constructed, but
  //       rather in every location where we construct such an object. Subtle difference, but important if we
  //       consider that the same line of code can instantiate 1000s of objects.
  def instantiate(types: List[Type]): A

  /**
    * Whether the schema can be instantiated with the given type arguments.
    */
  // def canInstantiate(types: List[Type]): Boolean
}
