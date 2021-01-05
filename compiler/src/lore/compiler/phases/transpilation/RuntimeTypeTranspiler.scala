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
    implicit val names: RuntimeTypeVariables = orderedVariables.map(tv => (tv, nameProvider.createName().name)).toMap
    val definitions = orderedVariables.declarationOrder.map { tv =>
      val varTypeVariable = names(tv)
      s"const $varTypeVariable = ${RuntimeApi.types.variable}('${tv.name}', ${transpile(tv.lowerBound)}, ${transpile(tv.upperBound)});"
    }
    (definitions.mkString("\n"), names)
  }

  /**
    * Transpiles the given type to its runtime representation. Any type variables need to be transpiled first using
    * [[transpileTypeVariables]], references to them being included in the implicit runtimeTypeVariables map.
    *
    * Since type variables aren't instantiated at run-time with this method, we do not need to simplify sum and
    * intersection types at run-time.
    */
  def transpile(tpe: Type)(implicit runtimeTypeVariables: RuntimeTypeVariables): JsExpr = {
    transpile(tpe, simplifyAtRuntime = false, tv => runtimeTypeVariables(tv))
  }

  /**
    * Transpiles the type to a run-time version where type variables are replaced with their actual assignments. Must
    * have access to a type variable assignment context such as [[TranspiledName.localTypeVariableAssignments]].
    *
    * Since type variables are resolved at run-time, we also have to simplify sum and intersection types to their
    * normal forms at run-time.
    */
  def transpileSubstitute(tpe: Type)(implicit runtimeTypeVariables: RuntimeTypeVariables): JsExpr = {
    transpile(tpe, simplifyAtRuntime = true, tv => {
      s"${RuntimeApi.utils.tinyMap.get}(${TranspiledName.localTypeVariableAssignments}, ${runtimeTypeVariables(tv)})"
    })
  }

  /**
    * Runtime simplification should only be done when absolutely necessary. Otherwise it will be a big draw on
    * performance.
    */
  private def transpile(tpe: Type, simplifyAtRuntime: Boolean, transpileTypeVariable: TypeVariable => JsExpr): JsExpr = {
    val rec: Type => JsExpr = t => transpile(t, simplifyAtRuntime, transpileTypeVariable)
    val api = RuntimeApi.types
    tpe match {
      case tv: TypeVariable => transpileTypeVariable(tv)
      case BasicType.Any => api.any
      case BasicType.Nothing => api.nothing
      case BasicType.Real => api.real
      case BasicType.Int => api.int
      case BasicType.Boolean => api.boolean
      case BasicType.String => api.string
      case ProductType.UnitType => RuntimeApi.tuples.unitType
      case declaredType: DeclaredType => TranspiledName.declaredType(declaredType).name
      case SumType(types) =>
        val sum = if (simplifyAtRuntime) RuntimeApi.sums.simplified else RuntimeApi.sums.tpe
        s"$sum([${types.map(rec).mkString(", ")}])"
      case IntersectionType(types) =>
        val intersection = if (simplifyAtRuntime) RuntimeApi.intersections.simplified else RuntimeApi.intersections.tpe
        s"$intersection([${types.map(rec).mkString(", ")}])"
      case ProductType(elements) => s"${RuntimeApi.tuples.tpe}([${elements.map(rec).mkString(", ")}])"
      case ListType(element) => s"${RuntimeApi.lists.tpe}(${rec(element)})"
      case MapType(key, value) => s"${RuntimeApi.maps.tpe}(${rec(key)}, ${rec(value)})"
      case ShapeType(properties) =>
        val propertyTypes = properties.values.map(property => s"${property.name}: ${rec(property.tpe)}")
        s"${RuntimeApi.shapes.tpe}({ ${propertyTypes.mkString(", ")} })"
    }
  }

}
