package lore.compiler.phases.transpilation

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
        case TypePath.TupleElement(index) => RuntimeApi.typePaths.tupleElement(expression, index)
        case TypePath.FunctionInput => RuntimeApi.typePaths.functionInput(expression)
        case TypePath.FunctionOutput => RuntimeApi.typePaths.functionOutput(expression)
        case TypePath.ListElement => RuntimeApi.typePaths.listElement(expression)
        case TypePath.MapKey => RuntimeApi.typePaths.mapKey(expression)
        case TypePath.MapValue => RuntimeApi.typePaths.mapValue(expression)
        case TypePath.ShapeProperty(name) =>
          // TODO (schemas): This isn't correct. The actual origin type at run time may actually be a struct, because
          //                 structs subtype shapes. So we have to deduce this at run time.
          RuntimeApi.typePaths.shapeProperty(expression, name)
        case TypePath.TypeArgument(schema, index) =>
          if (schema.isInstanceOf[StructSchema]) RuntimeApi.typePaths.structTypeArgument(expression, index)
          else ??? // TODO (schemas): If the accessed type argument is part of a trait, we have to use `findSubtype` at
                   //                 run time, because the actual origin type at run time may be a subtype of the trait.
      }
    }
  }

}
