package lore.compiler.transpilation

import lore.compiler.core.CompilationException
import lore.compiler.target.Target.TargetExpression
import lore.compiler.types.{StructSchema, TypePath}

object TypePathTranspiler {

  /**
    * Transpiles an access on the given `origin` type using the steps provided by the type path.
    */
  def transpileAccess(origin: TargetExpression, typePath: TypePath): TargetExpression = {
    typePath.steps.foldLeft(origin) { case (expression, step) =>
      step match {
        case TypePath.Identity => expression
        case TypePath.Part(_) => throw CompilationException("Cannot transpile `TypePath.Part` steps.")
        case TypePath.TupleElement(index) => RuntimeApi.types.typePaths.tupleElement(expression, index)
        case TypePath.FunctionInput => RuntimeApi.types.typePaths.functionInput(expression)
        case TypePath.FunctionOutput => RuntimeApi.types.typePaths.functionOutput(expression)
        case TypePath.ListElement => RuntimeApi.types.typePaths.listElement(expression)
        case TypePath.MapKey => RuntimeApi.types.typePaths.mapKey(expression)
        case TypePath.MapValue => RuntimeApi.types.typePaths.mapValue(expression)
        case TypePath.ShapeProperty(name) => RuntimeApi.types.typePaths.shapeProperty(expression, name)
        case TypePath.TypeArgument(schema, index) =>
          if (schema.isInstanceOf[StructSchema]) {
            RuntimeApi.types.typePaths.structTypeArgument(expression, index)
          } else {
            // If the accessed type argument is part of a trait, we have to use `findSubtype` at run time, because the
            // actual origin type at run time will be a subtype of the trait in form of a struct.
            RuntimeApi.types.typePaths.typeArgument(expression, RuntimeNames.schema(schema), index)
          }
      }
    }
  }

}
