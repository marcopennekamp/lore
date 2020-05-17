package lore.compiler.phases.transpilation

import lore.compiler.types.{ComponentType, DeclaredType}
import lore.types.{AnyType, BasicType, IntersectionType, ListType, MapType, NothingType, ProductType, SumType, Type}

object RuntimeTypeTranspiler {
  /**
    * Transpiles the given type into its runtime representation.
    */
  def transpile(tpe: Type): String = {
    tpe match {
      case AnyType => "Types.any"
      case NothingType => "Types.nothing"
      case BasicType.Real => "Types.real"
      case BasicType.Int => "Types.int"
      case BasicType.Boolean => "Types.boolean"
      case BasicType.String => "Types.string"
      case d: DeclaredType => s"Types.declared('${d.name}')"
      case IntersectionType(types) => s"Types.intersection([${types.map(transpile).mkString(", ")}])"
      case SumType(types) => s"Types.sum([${types.map(transpile).mkString(", ")}])"
      case ProductType(types) => s"Types.product([${types.map(transpile).mkString(", ")}])"
      case ComponentType(underlying) => s"Types.component(${transpile(underlying)})"
      case ListType(element) => s"Types.list(${transpile(element)})"
      case MapType(key, value) => s"Types.map(${transpile(key)}, ${transpile(value)})"
    }
  }
}
