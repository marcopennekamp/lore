package lore.compiler.semantics

import lore.compiler.semantics.functions.CoreMultiFunction
import lore.compiler.types.{BasicType, TupleType}

/**
  * This object contains all core definitions of Pyramid's `lore.Core` module. The compiler expects these definitions
  * to exist somewhere in the source code with compatible parameters and output types.
  */
object Core {
  private val modulePath: NamePath = NamePath("lore", "Core")

  val equal: CoreMultiFunction = CoreMultiFunction(modulePath + "equal?", TupleType(BasicType.Any, BasicType.Any), BasicType.Boolean)
  val less_than: CoreMultiFunction = CoreMultiFunction(modulePath + "less_than?", TupleType(BasicType.Any, BasicType.Any), BasicType.Boolean)
  val less_than_equal: CoreMultiFunction = CoreMultiFunction(modulePath + "less_than_equal?", TupleType(BasicType.Any, BasicType.Any), BasicType.Boolean)
  val hash: CoreMultiFunction = CoreMultiFunction(modulePath + "hash", TupleType(BasicType.Any), BasicType.Int)
  val to_string: CoreMultiFunction = CoreMultiFunction(modulePath + "to_string", TupleType(BasicType.Any), BasicType.String)

  val multiFunctions: Vector[CoreMultiFunction] = Vector(equal, less_than, less_than_equal, hash, to_string)
}
