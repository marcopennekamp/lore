package lore.compiler.transpilation.structures

import lore.compiler.transpilation.TypeTranspiler.RuntimeTypeVariables
import lore.compiler.transpilation.values.SymbolHistory
import lore.compiler.transpilation.{RuntimeApi, RuntimeNames, TypeTranspiler}
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
