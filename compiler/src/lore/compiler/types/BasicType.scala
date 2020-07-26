package lore.compiler.types

sealed abstract class BasicType(override val name: String) extends NamedType {
  override val hashCode: Int = name.hashCode
}

object BasicType {
  // TODO: There is a slight problem with how Reals and Ints are handled. Invariant classes: If we have a C[Real],
  //       we technically aren't able to "apply" a 5 to it, because 5 is strictly an Int. So clearly we have to type
  //       values such as 5 as Real AND Int, while 5.1 is typed only as Real.
  case object Real extends BasicType("Real")
  case object Int extends BasicType("Int")
  case object Boolean extends BasicType("Boolean")
  case object String extends BasicType("String")
}
