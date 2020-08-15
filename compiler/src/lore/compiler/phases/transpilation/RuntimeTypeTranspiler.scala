package lore.compiler.phases.transpilation

import lore.compiler.phases.transpilation.TranspiledChunk.{JsCode, JsExpr}
import lore.compiler.types.TypeExtensions._
import lore.compiler.types._

object RuntimeTypeTranspiler {
  type RuntimeTypeVariables = Map[TypeVariable, JsExpr]

  /**
    * Transpiles type variables such that they are defined as constants in the returned JSCode. The names of these
    * constants are defined in the returned map.
    */
  def transpileTypeVariables(variables: List[TypeVariable])(implicit nameProvider: TemporaryNameProvider): (JsCode, RuntimeTypeVariables) = {
    val orderedVariables = variables.declarationOrder
    implicit val names: RuntimeTypeVariables = orderedVariables.map(tv => (tv, nameProvider.createName())).toMap
    val definitions = orderedVariables.declarationOrder.map { tv =>
      val varTypeVariable = names(tv)
      s"const $varTypeVariable = ${RuntimeApi.types.variable}('${tv.name}', ${transpile(tv.lowerBound)}, ${transpile(tv.upperBound)});"
    }
    (definitions.mkString("\n"), names)
  }

  /**
    * Transpiles the given type to its runtime representation. Any type variables need to be transpiled first using
    * [[transpileTypeVariables]], references to them being included in the implicit runtimeTypeVariables map.
    */
  def transpile(tpe: Type)(implicit runtimeTypeVariables: RuntimeTypeVariables): JsExpr = transpile(tpe, tv => runtimeTypeVariables(tv))

  /**
    * Transpiles the type to a runtime version where type variables are replaced with their actual assignments. Must
    * have access to a type variable assignment context such as [[TranspiledNames.localTypeVariableAssignments]].
    */
  def transpileSubstitute(tpe: Type)(implicit runtimeTypeVariables: RuntimeTypeVariables): JsExpr = transpile(tpe, tv => {
    s"${RuntimeApi.utils.tinyMap.get}(${TranspiledNames.localTypeVariableAssignments}, ${runtimeTypeVariables(tv)})"
  })

  private def transpile(tpe: Type, transpileTypeVariable: TypeVariable => JsExpr): JsExpr = {
    val rec: Type => JsExpr = t => transpile(t, transpileTypeVariable)
    val api = RuntimeApi.types
    tpe match {
      case tv: TypeVariable => transpileTypeVariable(tv)
      case BasicType.Any => api.any
      case BasicType.Nothing => api.nothing
      case BasicType.Real => api.real
      case BasicType.Int => api.int
      case BasicType.Boolean => api.boolean
      case BasicType.String => api.string
      case ProductType.UnitType => api.unit
      case declaredType: DeclaredType => TranspiledNames.namedType(declaredType)
      // TODO: Now that sum and intersection types can be constructed at run-time using type variable assignments, we
      //       have to also introduce sum and intersection type simplifications in the run-time. However, we only have
      //       to apply these simplifications when the type contains a type variable and they are substituted.
      case SumType(types) => s"${api.sum}([${types.map(rec).mkString(", ")}])"
      case IntersectionType(types) => s"${api.intersection}([${types.map(rec).mkString(", ")}])"
      case ProductType(types) => s"${api.product}([${types.map(rec).mkString(", ")}])"
      case ListType(element) => s"${api.list}(${rec(element)})"
      case MapType(key, value) => s"${api.map}(${rec(key)}, ${rec(value)})"
      case ComponentType(underlying) => s"${api.component}(${rec(underlying)})"
    }
  }
}
