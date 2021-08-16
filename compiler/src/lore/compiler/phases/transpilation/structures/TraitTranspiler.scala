package lore.compiler.phases.transpilation.structures

import lore.compiler.phases.transpilation.TypeTranspiler.RuntimeTypeVariables
import lore.compiler.phases.transpilation.values.SymbolHistory
import lore.compiler.phases.transpilation.{RuntimeApi, RuntimeNames, TypeTranspiler}
import lore.compiler.target.Target.TargetExpression
import lore.compiler.types.TraitSchema

case class TraitTranspiler(schema: TraitSchema)(
  implicit symbolHistory: SymbolHistory,
) extends DeclaredSchemaTranspiler.SchemaTranspiler[TraitSchema] {

  /**
    * Transpiles a trait schema to its target representation.
    *
    * The trait's inherited shape type is also supplied to the runtime to support trait/shape subtyping.
    */
  override def transpileSchemaExpression(
    typeParameters: Vector[TargetExpression],
    supertraits: Vector[TargetExpression],
  )(implicit runtimeTypeVariables: RuntimeTypeVariables): TargetExpression = {
    val inheritedShapeTypeExpression = RuntimeApi.utils.`lazy`.of(TypeTranspiler.transpile(schema.inheritedShapeType))
    RuntimeApi.traits.schema(schema.name, typeParameters, supertraits, schema.hasMultipleParameterizedInheritance, inheritedShapeTypeExpression)
  }

  override def transpileSchemaInstantiation(typeArguments: TargetExpression): TargetExpression = {
    RuntimeApi.traits.tpe(RuntimeNames.schema(schema), typeArguments)
  }

}
