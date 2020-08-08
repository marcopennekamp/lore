package lore.compiler.phases.transpilation

import lore.compiler.types.TypeExtensions._
import lore.compiler.types._

object RuntimeTypeTranspiler {
  /**
    * Transpiles the given type to its runtime representation.
    */
  def transpile(rootType: Type)(implicit nameProvider: TemporaryNameProvider): TranspiledChunk = {
    val typesApi = RuntimeApi.types
    val variables = Type.variables(rootType).declarationOrder
    val variableNames = variables.map(tv => (tv, nameProvider.createName())).toMap

    def rec(tpe: Type): String = {
      tpe match {
        case tv: TypeVariable => variableNames(tv)
        case _ => tpe match {
          case BasicType.Any => typesApi.any
          case BasicType.Nothing => typesApi.nothing
          case BasicType.Real => typesApi.real
          case BasicType.Int => typesApi.int
          case BasicType.Boolean => typesApi.boolean
          case BasicType.String => typesApi.string
          case ProductType.UnitType => typesApi.unit
          case declaredType: DeclaredType => TranspiledNames.namedType(declaredType)
          case IntersectionType(types) => s"${typesApi.intersection}([${types.map(rec).mkString(", ")}])"
          case SumType(types) => s"${typesApi.sum}([${types.map(rec).mkString(", ")}])"
          case ProductType(types) => s"${typesApi.product}([${types.map(rec).mkString(", ")}])"
          case ComponentType(underlying) => s"${typesApi.component}(${rec(underlying)})"
          case ListType(element) => s"${typesApi.list}(${rec(element)})"
          case MapType(key, value) => s"${typesApi.map}(${rec(key)}, ${rec(value)})"
        }
      }
    }

    val variableConstructions = variables.map { tv =>
      val varTypeVariable = variableNames(tv)
      s"const $varTypeVariable = ${typesApi.variable}('${tv.name}', ${rec(tv.lowerBound)}, ${rec(tv.upperBound)});"
    }
    TranspiledChunk.chunk(variableConstructions, rec(rootType))
  }
}
