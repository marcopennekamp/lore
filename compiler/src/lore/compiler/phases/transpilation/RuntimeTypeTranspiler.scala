package lore.compiler.phases.transpilation

import lore.compiler.types.{ComponentType, DeclaredType}
import lore.types.{AnyType, BasicType, IntersectionType, ListType, MapType, NothingType, ProductType, SumType, Type}

object RuntimeTypeTranspiler {
  /**
    * Transpiles the given type into its runtime representation.
    */
  def transpile(tpe: Type): String = {
    s"${LoreApi.varTypes}." + (tpe match {
      case AnyType => "any"
      case NothingType => "nothing"
      case BasicType.Real => "real"
      case BasicType.Int => "int"
      case BasicType.Boolean => "boolean"
      case BasicType.String => "string"
      case d: DeclaredType => s"declared('${d.name}')"
      case IntersectionType(types) => s"intersection([${types.map(transpile).mkString(", ")}])"
      case SumType(types) => s"sum([${types.map(transpile).mkString(", ")}])"
      case ProductType(types) => s"product([${types.map(transpile).mkString(", ")}])"
      case ComponentType(underlying) => s"component(${transpile(underlying)})"
      case ListType(element) => s"list(${transpile(element)})"
      case MapType(key, value) => s"map(${transpile(key)}, ${transpile(value)})"
    })
  }
}
