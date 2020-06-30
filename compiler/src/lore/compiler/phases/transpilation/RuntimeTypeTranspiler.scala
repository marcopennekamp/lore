package lore.compiler.phases.transpilation

import lore.compiler.types.{ComponentType, DeclaredType, TypeVariable}
import lore.types._
import lore.compiler.types.TypeExtensions._

object RuntimeTypeTranspiler {
  /**
    * Transpiles the given type to its runtime representation.
    */
  def transpile(rootType: Type)(implicit nameProvider: TemporaryNameProvider): TranspiledChunk = {
    val apiPrefix = s"${LoreApi.varTypes}."
    val variables = Type.variables(rootType).declarationOrder
    val variableNames = variables.map(tv => (tv, nameProvider.createName())).toMap

    def rec(tpe: Type): String = {
      tpe match {
        case tv: TypeVariable => variableNames(tv)
        case _ => apiPrefix + (tpe match {
          case AnyType => "any"
          case NothingType => "nothing"
          case BasicType.Real => "real"
          case BasicType.Int => "int"
          case BasicType.Boolean => "boolean"
          case BasicType.String => "string"
          case ProductType.UnitType => s"unit"
          case d: DeclaredType => s"declared('${d.name}')"
          case IntersectionType(types) => s"intersection([${types.map(rec).mkString(", ")}])"
          case SumType(types) => s"sum([${types.map(rec).mkString(", ")}])"
          case ProductType(types) => s"product([${types.map(rec).mkString(", ")}])"
          case ComponentType(underlying) => s"component(${rec(underlying)})"
          case ListType(element) => s"list(${rec(element)})"
          case MapType(key, value) => s"map(${rec(key)}, ${rec(value)})"
        })
      }
    }

    val variableConstructions = variables.map { tv =>
      val varTypeVariable = variableNames(tv)
      s"const $varTypeVariable = ${apiPrefix}variable('${tv.name}', ${rec(tv.lowerBound)}, ${rec(tv.upperBound)});"
    }
    TranspiledChunk.chunk(variableConstructions, rec(rootType))
  }
}
