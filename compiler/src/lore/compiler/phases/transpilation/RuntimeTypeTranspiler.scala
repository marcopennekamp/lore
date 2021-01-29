package lore.compiler.phases.transpilation

import lore.compiler.target.Target
import lore.compiler.target.Target.{TargetExpression, TargetStatement}
import lore.compiler.target.TargetDsl.{StringExtension, VariableExtension}
import lore.compiler.types._

// TODO: Rename to TypeTranspiler.
object RuntimeTypeTranspiler {

  type TranspiledTypeVariables = Map[TypeVariable, Target.Variable]

  /**
    * Transpiles type variables such that they are defined as constants in the returned target statement. The names of
    * these constants are defined in the returned map.
    */
  def transpileTypeVariables(variables: Vector[TypeVariable])(implicit nameProvider: TemporaryNameProvider): (Vector[TargetStatement], TranspiledTypeVariables) = {
    val orderedVariables = variables.sortBy(_.declarationOrder)
    implicit val transpiledVariables: TranspiledTypeVariables = orderedVariables.map(tv => (tv, nameProvider.createName().asVariable)).toMap
    val definitions = orderedVariables.map { tv =>
      transpiledVariables(tv).declareAs(RuntimeApi.types.variable(tv.name, transpile(tv.lowerBound), transpile(tv.upperBound)))
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
  def transpile(tpe: Type)(implicit typeVariables: TranspiledTypeVariables): TargetExpression = {
    transpile(tpe, simplifyAtRuntime = false, tv => typeVariables(tv))
  }

  /**
    * Transpiles the type to a run-time version where type variables are replaced with their actual assignments. Must
    * have access to a type variable assignment context such as [[TranspiledName.localTypeVariableAssignments]].
    *
    * Since type variables are resolved at run-time, we also have to simplify sum and intersection types to their
    * normal forms at run-time.
    */
  def transpileSubstitute(tpe: Type)(implicit typeVariables: TranspiledTypeVariables): TargetExpression = {
    transpile(tpe, simplifyAtRuntime = true, tv => {
      RuntimeApi.utils.tinyMap.get(TranspiledName.localTypeVariableAssignments.asVariable, typeVariables(tv))
    })
  }

  /**
    * Runtime simplification should only be performed when absolutely necessary. Otherwise it will be a big draw on
    * performance.
    */
  private def transpile(tpe: Type, simplifyAtRuntime: Boolean, transpileTypeVariable: TypeVariable => TargetExpression): TargetExpression = {
    val rec: Type => TargetExpression = t => transpile(t, simplifyAtRuntime, transpileTypeVariable)
    val api = RuntimeApi.types
    tpe match {
      // TODO (alias): Transpile type aliases by referring to them by name at run-time. We cannot use a global Type -> Name map
      //       in the registry (as would be intuitive to avoid adding an alias property to Type), because we could for
      //       example define two type aliases A = { name: String } and B = { name: String }. If we transpiled types
      //       to their alias representations globally, we could not decide whether A or B had been mentioned. This
      //       does not make a difference to the execution, but could become a failure point if we introduced
      //       module-based compilation. And in any case, the generated code would be conceptually incorrect, if we
      //       referred to A in one place where actually B was referenced in the Lore source.
      //       Adding to this, we also have to take care that some types shouldn't be transpiled as aliases. For
      //       example, if we have an alias A = S, where S is a struct, we should just substitute S for A without
      //       transpiling the alias. So only "constructed" types should be able to receive an alias, not named types.
      //       If we add an alias property to types, we will also have to ensure that this does not affect type
      //       equality.
      //       Also note that sometimes types may be normalized, such as combined shapes in intersection types or
      //       flattened sums, and such new types shouldn't ever refer to the alias used to build the type.
      case tv: TypeVariable => transpileTypeVariable(tv)
      case BasicType.Any => api.any
      case BasicType.Nothing => api.nothing
      case BasicType.Real => api.real
      case BasicType.Int => api.int
      case BasicType.Boolean => api.boolean
      case BasicType.String => api.string
      case ProductType.UnitType => RuntimeApi.tuples.unitType
      case declaredType: DeclaredType => TranspiledName.declaredType(declaredType).asVariable
      case SumType(types) =>
        val args = types.map(rec).toVector
        if (simplifyAtRuntime) RuntimeApi.sums.simplified(args) else RuntimeApi.sums.tpe(args)
      case IntersectionType(types) =>
        val args = types.map(rec).toVector
        if (simplifyAtRuntime) RuntimeApi.intersections.simplified(args) else RuntimeApi.intersections.tpe(args)
      case ProductType(elements) => RuntimeApi.tuples.tpe(elements.map(rec))
      case ListType(element) => RuntimeApi.lists.tpe(rec(element))
      case MapType(key, value) => RuntimeApi.maps.tpe(rec(key), rec(value))
      case ShapeType(properties) =>
        RuntimeApi.shapes.tpe(Target.Dictionary(
          properties.values.toVector.map(property => Target.Property(property.name.asName, rec(property.tpe)))
        ))
    }
  }

}
