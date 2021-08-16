package lore.compiler.phases.transpilation

import lore.compiler.core.CompilationException
import lore.compiler.phases.transpilation.structures.DeclaredSchemaTranspiler
import lore.compiler.phases.transpilation.values.SymbolHistory
import lore.compiler.target.Target
import lore.compiler.target.Target.{TargetExpression, TargetStatement}
import lore.compiler.target.TargetDsl.{ExpressionExtension, StringExtension, VariableExtension}
import lore.compiler.types._

object TypeTranspiler {

  case class RuntimeTypeVariable(tv: TypeVariable, index: Int, expression: Target.Variable)

  type RuntimeTypeVariables = Map[TypeVariable, RuntimeTypeVariable]

  /**
    * Transpiles type variables such that they are defined as constants in the returned target statement. The access
    * expressions of these constants are defined in the returned runtime type variable map.
    *
    * The variable list must be ordered in the declaration order of the variables. At run time, the order of the type
    * variables will determine at which index their instantiations will be placed in an assignments array.
    */
  def transpileTypeVariables(typeVariables: Vector[TypeVariable])(implicit variableProvider: TemporaryVariableProvider, symbolHistory: SymbolHistory): (Vector[TargetStatement], RuntimeTypeVariables) = {
    implicit val runtimeTypeVariables: RuntimeTypeVariables = typeVariables.zipWithIndex.map {
      case (tv, index) => (tv, RuntimeTypeVariable(tv, index, variableProvider.createVariable()))
    }.toMap

    val definitions = typeVariables.map { tv =>
      val rtv = runtimeTypeVariables(tv)
      rtv.expression.declareAs(RuntimeApi.types.variable(tv.name, rtv.index, transpile(tv.lowerBound), transpile(tv.upperBound), tv.variance))
    }

    (definitions, runtimeTypeVariables)
  }

  /**
    * Transpiles the given type to its runtime representation. Any type variables need to be transpiled first using
    * [[transpileTypeVariables]], references to them being included in the implicit runtimeTypeVariables map.
    *
    * Since type variables aren't instantiated at run-time with this method, we do not need to simplify sum and
    * intersection types at run-time.
    */
  def transpile(tpe: Type)(implicit runtimeTypeVariables: RuntimeTypeVariables, symbolHistory: SymbolHistory): TargetExpression = {
    transpile(tpe, simplifyAtRuntime = false, tv => runtimeTypeVariables(tv).expression)
  }

  /**
    * Transpiles the type to a run-time version where type variables are replaced with their actual assignments. Must
    * have access to a type variable assignment context such as [[RuntimeNames.localTypeVariableAssignments]].
    *
    * Since type variables are resolved at run-time, we also have to simplify sum and intersection types to their
    * normal forms at run-time.
    *
    * If the given type contains no type variables, it is transpiled without run-time simplification.
    */
  def transpileSubstitute(tpe: Type)(implicit runtimeTypeVariables: RuntimeTypeVariables, symbolHistory: SymbolHistory): TargetExpression = {
    if (Type.isPolymorphic(tpe)) {
      transpile(tpe, simplifyAtRuntime = true, tv => {
        val rtv = runtimeTypeVariables(tv)
        RuntimeNames.localTypeVariableAssignments.element(Target.IntLiteral(rtv.index))
      })
    } else {
      transpile(tpe, simplifyAtRuntime = false, _ => throw CompilationException(s"The given type $tpe was determined to be monomorphic."))
    }
  }

  /**
    * Runtime simplification should only be performed when absolutely necessary. Otherwise it will be a big draw on
    * performance.
    */
  private def transpile(tpe: Type, simplifyAtRuntime: Boolean, transpileTypeVariable: TypeVariable => TargetExpression)(implicit symbolHistory: SymbolHistory): TargetExpression = {
    val rec: Type => TargetExpression = t => transpile(t, simplifyAtRuntime, transpileTypeVariable)
    val api = RuntimeApi.types
    tpe match {
      case tv: TypeVariable => transpileTypeVariable(tv)
      case BasicType.Any => api.any
      case BasicType.Nothing => api.nothing
      case BasicType.Real => api.real
      case BasicType.Int => api.int
      case BasicType.Boolean => api.boolean
      case BasicType.String => api.string
      case TupleType.UnitType => RuntimeApi.tuples.unitType
      case dt: DeclaredType => transpileSchemaInstantiation(rec)(dt.schema, dt.typeArguments)
      case SumType(types) =>
        val args = types.map(rec).toVector
        if (simplifyAtRuntime) RuntimeApi.sums.simplified(args) else RuntimeApi.sums.tpe(args)
      case IntersectionType(types) =>
        val args = types.map(rec).toVector
        if (simplifyAtRuntime) RuntimeApi.intersections.simplified(args) else RuntimeApi.intersections.tpe(args)
      case TupleType(elements) => RuntimeApi.tuples.tpe(elements.map(rec))
      case FunctionType(input, output) => RuntimeApi.functions.tpe(rec(input), rec(output))
      case ListType(element) => RuntimeApi.lists.tpe(rec(element))
      case MapType(key, value) => RuntimeApi.maps.tpe(rec(key), rec(value))
      case ShapeType(properties) =>
        RuntimeApi.shapes.tpe(Target.Dictionary(
          properties.values.toVector.map(property => Target.Property(property.name, rec(property.tpe)))
        ))
      case SymbolType(name) => symbolHistory.targetType(name)
    }
  }

  /**
    * Transpiles an instantiation of the given declared schema with the given type arguments. This is used to
    * transpile declared types known at compile time. If the schema is constant, the representative of the type is
    * returned.
    *
    * Note that even though struct types can also contain open property types, this function will never be able to
    * know about any of them, because it transpiles declared types known at compile time. Hence, a struct type without
    * type arguments but with potential open properties will still compile down to its representative.
    */
  private def transpileSchemaInstantiation(rec: Type => TargetExpression)(
    schema: DeclaredSchema,
    typeArguments: Vector[Type],
  )(implicit symbolHistory: SymbolHistory): TargetExpression = {
    if (schema.isConstant) {
      return RuntimeNames.schema.representative(schema)
    }
    DeclaredSchemaTranspiler.getSchemaTranspiler(schema).transpileSchemaInstantiation(Target.List(typeArguments.map(rec)))
  }

}
