package lore.compiler.phases.transpilation

import lore.compiler.core.CompilationException
import lore.compiler.phases.transpilation.values.SymbolHistory
import lore.compiler.target.Target
import lore.compiler.target.Target.{TargetExpression, TargetStatement}
import lore.compiler.target.TargetDsl.{StringExtension, VariableExtension}
import lore.compiler.types._

object TypeTranspiler {

  type TranspiledTypeVariables = Map[TypeVariable, Target.Variable]

  /**
    * Transpiles type variables such that they are defined as constants in the returned target statement. The names of
    * these constants are defined in the returned map.
    *
    * The variable list must be ordered in the declaration order of the variables.
    */
  def transpileTypeVariables(typeVariables: Vector[TypeVariable])(implicit variableProvider: TemporaryVariableProvider, symbolHistory: SymbolHistory): (Vector[TargetStatement], TranspiledTypeVariables) = {
    implicit val transpiledVariables: TranspiledTypeVariables = typeVariables.map(tv => (tv, variableProvider.createVariable())).toMap
    val definitions = typeVariables.map { tv =>
      transpiledVariables(tv).declareAs(RuntimeApi.types.variable(tv.name, transpile(tv.lowerBound), transpile(tv.upperBound), tv.variance))
    }
    (definitions, transpiledVariables)
  }

  /**
    * Transpiles the given type to its runtime representation. Any type variables need to be transpiled first using
    * [[transpileTypeVariables]], references to them being included in the implicit typeVariables map.
    *
    * Since type variables aren't instantiated at run-time with this method, we do not need to simplify sum and
    * intersection types at run-time.
    */
  def transpile(tpe: Type)(implicit typeVariables: TranspiledTypeVariables, symbolHistory: SymbolHistory): TargetExpression = {
    transpile(tpe, simplifyAtRuntime = false, tv => typeVariables(tv))
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
  def transpileSubstitute(tpe: Type)(implicit typeVariables: TranspiledTypeVariables, symbolHistory: SymbolHistory): TargetExpression = {
    if (Type.isPolymorphic(tpe)) {
      transpile(tpe, simplifyAtRuntime = true, tv => {
        RuntimeApi.utils.tinyMap.get(RuntimeNames.localTypeVariableAssignments, typeVariables(tv))
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
      case declaredType: DeclaredType => RuntimeNames.declaredType(declaredType) // TODO (schemas): Access interned types if the declared schema is not constant.
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
          properties.values.toVector.map(property => Target.Property(property.name.asName, rec(property.tpe)))
        ))
      case SymbolType(name) => symbolHistory.targetType(name)
    }
  }

}
