package lore.compiler.phases.transpilation.structures

import lore.compiler.core.CompilationException
import lore.compiler.phases.transpilation.TypeTranspiler.RuntimeTypeVariables
import lore.compiler.phases.transpilation.values.SymbolHistory
import lore.compiler.phases.transpilation.{RuntimeNames, TemporaryVariableProvider, TypeTranspiler}
import lore.compiler.semantics.Registry
import lore.compiler.target.Target.{TargetExpression, TargetStatement}
import lore.compiler.target.TargetDsl.{ExpressionExtension, VariableExtension}
import lore.compiler.types.{DeclaredSchema, StructSchema, TraitSchema}

object DeclaredSchemaTranspiler {

  trait SchemaTranspiler[+A <: DeclaredSchema] {
    def schema: A

    def transpileSchemaExpression(
      typeParameters: Vector[TargetExpression],
      supertraits: Vector[TargetExpression],
    )(implicit runtimeTypeVariables: RuntimeTypeVariables): TargetExpression

    /**
      * Transpiles a schema instantiation to create an appropriate type instance. This function is only called if the
      * schema isn't constant.
      */
    def transpileSchemaInstantiation(typeArguments: TargetExpression): TargetExpression

    def transpileAdditionalDeclarations()(implicit runtimeTypeVariables: RuntimeTypeVariables, registry: Registry): Vector[TargetStatement] = Vector.empty
    def transpiledDeferredDeclarations(): Vector[TargetStatement] = Vector.empty
  }

  /**
    * Transpiles the given declared schema and definition to its representation. Any schemas this schema eagerly
    * depends on must be transpiled before this type is transpiled.
    *
    * A transpiled declared schema consists of the following parts:
    *
    *   1. The type schema which saves the information (type parameters, property types, inherited shape type, etc.)
    *      common to all instances of the declared type, as well as its representative. If the declared schema is
    *      constant, the representative is referred to in all static uses (such as the right-hand type in multiple
    *      dispatch).
    *
    * In addition to these, traits and structs may each transpile additional declarations.
    */
  def transpile(schema: DeclaredSchema)(implicit registry: Registry, symbolHistory: SymbolHistory): Vector[TargetStatement] = {
    val schemaTranspiler = getSchemaTranspiler(schema)
    implicit val variableProvider: TemporaryVariableProvider = new TemporaryVariableProvider(s"${RuntimeNames.schema(schema).name}__")
    implicit val (typeParameterStatements, runtimeTypeVariables) = TypeTranspiler.transpileTypeVariables(schema.parameters, schema.name)

    val typeParameterExpressions = runtimeTypeVariables.values.map(_.expression).toVector
    val supertraitExpressions = schema.declaredSupertypes.map(TypeTranspiler.transpile)

    val varSchema = RuntimeNames.schema(schema)
    val varRepresentative = RuntimeNames.schema.representative(schema)
    val schemaDeclaration = Vector(
      varSchema.declareAs(
        schemaTranspiler.transpileSchemaExpression(typeParameterExpressions, supertraitExpressions)
      ),
      varRepresentative.declareAs(RuntimeNames.schema(schema).prop("representative")),
    )

    val additionalDeclarations = schemaTranspiler.transpileAdditionalDeclarations()

    typeParameterStatements ++ schemaDeclaration ++ additionalDeclarations
  }

  /**
    * Transpiles any additional definitions of the given declared schema that could depend on ANY schema regardless of
    * schema order. These have to be initialized after all schemas have been initialized, hence the "deferred" moniker.
    */
  def transpileDeferred(schema: DeclaredSchema)(implicit registry: Registry, symbolHistory: SymbolHistory): Vector[TargetStatement] = {
    getSchemaTranspiler(schema).transpiledDeferredDeclarations()
  }

  def getSchemaTranspiler(schema: DeclaredSchema)(implicit symbolHistory: SymbolHistory): SchemaTranspiler[DeclaredSchema] = {
    schema match {
      case structSchema: StructSchema => StructTranspiler(structSchema)
      case traitSchema: TraitSchema => TraitTranspiler(traitSchema)
      case _ => throw CompilationException(s"Unknown declared schema $schema.")
    }
  }

}
