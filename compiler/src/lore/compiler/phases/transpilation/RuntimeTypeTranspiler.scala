package lore.compiler.phases.transpilation

import lore.compiler.types.{ComponentType, DeclaredType, TypeVariable}
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
      // TODO: If a type variable occurs multiple times, this code would lead to different type variables,
      //       because we create a new instance at every usage point (recall reference equality for type variables).
      //       Rather, we need to create a "type variable context" in FunctionTranspilationVisitor that holds the
      //       created type variable for us to re-use. (We can follow a "getOrCreate" scheme so that we don't need
      //       to traverse the type twice.)
      case tv: TypeVariable => s"variable(${tv.name}, ${transpile(tv.lowerBound)}, ${transpile(tv.upperBound)})"
    })
  }
}
