package lore.compiler.transpilation.structures

import lore.compiler.core.CompilationException
import lore.compiler.semantics.Registry
import lore.compiler.semantics.structures.StructPropertyDefinition
import lore.compiler.target.Target
import lore.compiler.target.Target.{TargetExpression, TargetStatement}
import lore.compiler.target.TargetDsl._
import lore.compiler.transpilation.TypeTranspiler.RuntimeTypeVariables
import lore.compiler.transpilation.expressions.ExpressionTranspiler
import lore.compiler.transpilation.values.SymbolHistory
import lore.compiler.transpilation.{RuntimeApi, RuntimeNames, TypePathTranspiler, TypeTranspiler}
import lore.compiler.types.{StructSchema, TypePath, TypeVariable}

case class StructTranspiler(schema: StructSchema)(
  implicit symbolHistory: SymbolHistory,
) extends DeclaredSchemaTranspiler.SchemaTranspiler[StructSchema] {

  /**
    * Transpiles a struct schema to its target representation.
    *
    * The information about the struct's properties is also supplied to the runtime to support struct/shape subtyping.
    */
  override def transpileSchemaExpression(
    typeParameters: Vector[Target.TargetExpression],
    supertraits: Vector[Target.TargetExpression],
  )(implicit runtimeTypeVariables: RuntimeTypeVariables): Target.TargetExpression = {
    val propertyTypes = Target.Dictionary(schema.definition.properties.map { property =>
      // As noted in the runtime's StructSchema definition, schema property types must be lazy to ensure that all
      // types are already initialized when the property type is initialized.
      Target.Property(property.name, RuntimeApi.utils.`lazy`.of(TypeTranspiler.transpile(property.tpe)))
    })
    val propertyOrder = schema.definition.properties.map(_.name)
    val openPropertyOrder = schema.definition.openProperties.map(_.name)
    RuntimeApi.structs.schema(schema.name, typeParameters, supertraits, schema.hasMultipleParameterizedInheritance, propertyTypes, propertyOrder, openPropertyOrder)
  }

  override def transpileSchemaInstantiation(typeArguments: TargetExpression): TargetExpression = {
    transpileSchemaInstantiation(Some(typeArguments), None)
  }

  private def transpileSchemaInstantiation(typeArguments: Option[TargetExpression], openPropertyTypes: Option[TargetExpression]): TargetExpression = {
    if (typeArguments.isEmpty && openPropertyTypes.isEmpty) {
      return RuntimeNames.schema.representative(schema)
    }

    val varSchema = RuntimeNames.schema(schema)
    RuntimeApi.structs.tpe(
      varSchema,
      typeArguments.getOrElse(Target.Undefined),
      openPropertyTypes.getOrElse(Target.Undefined),
    )
  }

  /**
    * Transpiles the following additional declarations:
    *
    *   1. A struct instantiation function which takes a properties object, and type arguments unless the schema is
    *      constant, computes the correct run-time type, and instantiates a value of the struct.
    *   2. A `construct` function which is a more convenient interface to the instantiation function. It takes the
    *      struct's type arguments (if parametric) as the first argument and all other constructor arguments in order.
    *      This function is used by all constructors.
    *   3. A function for each property's default value. This function is invoked to generate another default values
    *      during instantiation.
    */
  override def transpileAdditionalDeclarations()(implicit runtimeTypeVariables: RuntimeTypeVariables, registry: Registry): Vector[TargetStatement] = {
    transpileStructInstantiate() ++ transpileStructConstruct() ++ transpileDefaultValues()
  }

  private def transpileStructInstantiate(): Vector[TargetStatement] = {
    val varInstantiate = RuntimeNames.struct.instantiate(schema)
    val paramProperties = "properties".asParameter
    val varProperties = paramProperties.asVariable
    val paramTypeArguments = "typeArguments".asParameter
    val varTypeArguments = paramTypeArguments.asVariable

    val openPropertyTypes = if (schema.hasOpenProperties) Some(getPropertyTypesDictionary(varProperties)) else None
    val typeArguments = if (!schema.isConstant) Some(varTypeArguments) else None
    val tpe = transpileSchemaInstantiation(typeArguments, openPropertyTypes)
    val parameters = if (schema.isConstant) Vector(paramProperties) else Vector(paramProperties, paramTypeArguments)

    // We need to override any type arguments for open type parameters with a run-time type argument.
    val openTypeArgumentAssignments = if (schema.openParameters.nonEmpty) {
      schema.parameters.zipWithIndex.filter(_._1.isOpen).map((getOpenTypeArgumentAssignment(varProperties, varTypeArguments) _).tupled)
    } else Vector.empty

    Vector(
      Target.Function(varInstantiate.name, parameters, Target.Block(
        openTypeArgumentAssignments ++ Vector(
          Target.Return(RuntimeApi.structs.value(paramProperties.asVariable, tpe)),
        ),
      ))
    )
  }

  /**
    * To instantiate a struct with the right type, the actual property types of the struct are retrieved using typeOf.
    */
  private def getPropertyTypesDictionary(varProperties: Target.Variable): Target.Dictionary = {
    getPropertyDictionary(schema.definition.openProperties, property => RuntimeApi.types.typeOf(varProperties.prop(property.name)))
  }

  /**
    * Transpiles the type argument deduction for open type parameters. We utilize [[TypePath]] to access the relevant
    * type directly.
    */
  private def getOpenTypeArgumentAssignment(
    varProperties: Target.Variable,
    varTypeArguments: Target.Variable,
  )(typeParameter: TypeVariable, index: Int): TargetStatement = {
    val property = schema.derivingProperties(typeParameter)
    val typePath = TypePath.of(property.tpe, typeParameter) match {
      case Vector(path) => path
      case _ => throw CompilationException(s"The type path to the open type parameter $typeParameter must exist and be unique. This is not the case. Schema: $schema.")
    }

    val target = varTypeArguments.element(Target.IntLiteral(index))
    val propertyType = RuntimeApi.types.typeOf(varProperties.prop(property.name))
    val tpe = TypePathTranspiler.transpileAccess(propertyType, typePath)

    Target.Assignment(target, tpe)
  }

  private def transpileStructConstruct(): Vector[TargetStatement] = {
    val varInstantiate = RuntimeNames.struct.instantiate(schema)
    val varConstruct = RuntimeNames.struct.construct(schema)
    val paramTypeArguments = "typeArguments".asParameter

    val typeArgumentParameters = if (!schema.isConstant) Vector(paramTypeArguments) else Vector.empty
    val propertyParameters = schema.definition.properties.map(property => RuntimeNames.localVariable(property.name).asParameter)
    val parameters = typeArgumentParameters ++ propertyParameters
    val properties = getPropertyDictionary(schema.definition.properties, property => RuntimeNames.localVariable(property.name))

    Vector(
      Target.Function(varConstruct.name, parameters, Target.block(
        Target.Return(Target.Call(varInstantiate, Vector(properties) ++ typeArgumentParameters.map(_.asVariable)))
      ))
    )
  }

  private def getPropertyDictionary(properties: Vector[StructPropertyDefinition], getPropertyExpression: StructPropertyDefinition => TargetExpression): Target.Dictionary = {
    val entries = properties.map {
      property => Target.Property(property.name, getPropertyExpression(property))
    }
    Target.Dictionary(entries)
  }

  private def transpileDefaultValues()(implicit runtimeTypeVariables: RuntimeTypeVariables, registry: Registry): Vector[TargetStatement] = {
    schema.definition.properties.flatMap(property => property.defaultValue.map(transpileDefaultValue(property, _)))
  }

  private def transpileDefaultValue(
    property: StructPropertyDefinition,
    defaultValue: StructPropertyDefinition.DefaultValue,
  )(implicit runtimeTypeVariables: RuntimeTypeVariables, registry: Registry): TargetStatement = {
    val varDefaultValue = RuntimeNames.struct.defaultValue(schema, property)
    val chunk = ExpressionTranspiler.transpile(defaultValue.expression)
    Target.Function(varDefaultValue.name, Vector.empty, chunk.asBody)
  }

  /**
    * Transpiles a call-style constructor function value which delegates to the instantiation function, if the schema
    * is constant.
    *
    * Due to initialization order constraints, the constructor function values must be transpiled after all declared
    * schemas have been transpiled. This is the only way to avoid having to lazily initialize the constructor's
    * function value.
    */
  override def transpiledDeferredDeclarations(): Vector[TargetStatement] = transpileConstructor() ++ transpileObject()

  /**
    * Transpiles a constructor for a constant struct schema. Constructors aren't transpiled for objects.
    *
    * Example:
    * {{{
    * const ABC__constructor = Lore.functions.value(
    *   ABC_construct,
    *   /* function type */,
    * );
    * }}}
    */
  private def transpileConstructor(): Vector[TargetStatement] = {
    if (!schema.isConstant || schema.definition.isObject) {
      return Vector.empty
    }

    // As the schema is constant, we can provide the empty map as runtime type variables.
    implicit val runtimeTypeVariables: RuntimeTypeVariables = Map.empty

    val varConstruct = RuntimeNames.struct.construct(schema)
    val varConstructor = RuntimeNames.struct.constructor(schema)
    val functionType = schema.representative.constructorSignature.functionType

    Vector(
      Target.VariableDeclaration(
        varConstructor.name,
        RuntimeApi.functions.value(varConstruct, TypeTranspiler.transpile(functionType))
      )
    )
  }

  /**
    * Transpiles the object instance declaration of a struct object. The object may be instantiated lazily if the
    * definition indicates so.
    */
  private def transpileObject(): Vector[TargetStatement] = {
    if (!schema.definition.isObject) {
      return Vector.empty
    }

    val varObject = RuntimeNames.struct.`object`(schema)
    val varConstruct = RuntimeNames.struct.construct(schema)
    val defaultValues = schema.definition.properties.map { property =>
      RuntimeNames.struct.defaultValue(schema, property).call()
    }
    val construction = varConstruct.call(defaultValues: _*)
    val declaration = varObject.declareAs(
      if (schema.definition.allDefaultsLocalized) construction
      else RuntimeApi.utils.`lazy`.of(construction)
    )

    Vector(declaration)
  }

}
