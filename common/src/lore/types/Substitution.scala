package lore.types

object Substitution {

  /**
    * In the given type, substitute all occurrences of any given variable with its respective type value.
    */
  def substitute(assignments: TypeVariable.Assignments, tpe: Type): Type = {
    def rec(t: Type) = substitute(assignments, t)

    tpe match {
      case tv: TypeVariable => assignments.get(tv) match {
        case None => tv
        case Some(t) => t
      }
      case ProductType(components) => ProductType(components.map(rec))
      case SumType(types) => SumType(types.map(rec))
      case IntersectionType(types) => IntersectionType(types.map(rec))
      case ListType(element) => ListType(rec(element))
      case MapType(key, value) => MapType(rec(key), rec(value))
      case t => t
    }
  }

}
