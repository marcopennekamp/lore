package lore.compiler.assembly.schemas

import lore.compiler.assembly.functions.{ConstructorAssembler, FunctionAssembler}
import lore.compiler.assembly.globals.GlobalVariableAssembler
import lore.compiler.assembly.types.{TypeAssembler, TypePathAssembler}
import lore.compiler.assembly.{Chunk, RuntimeNames, PropertyOrder, RegisterProvider}
import lore.compiler.core.CompilationException
import lore.compiler.poem._
import lore.compiler.semantics.Registry
import lore.compiler.semantics.functions.FunctionSignature
import lore.compiler.semantics.structures.StructPropertyDefinition
import lore.compiler.types.{StructSchema, TypePath}

object StructSchemaAssembler {

  /**
    * Generates a PoemStructSchema for the given schema, any auxiliary functions the schema requires (including the
    * constructor), and a PoemGlobalVariable for the object.
    */
  def generate(
    schema: StructSchema,
    poemTypeParameters: Vector[PoemTypeParameter],
    poemSupertraits: Vector[PoemNamedType],
  )(implicit registry: Registry): (PoemStructSchema, Vector[PoemFunction], Option[PoemGlobalVariable]) = {
    val poemProperties = generateProperties(schema.properties)
    val poemSchema = PoemStructSchema(schema.kind, schema.name, poemTypeParameters, poemSupertraits, poemProperties)
    val poemConstructor = generateConstructor(schema)
    val (poemObject, poemObjectFunctions) = if (schema.isObject) {
      val (poemGlobalVariable, poemFunctions) = generateObject(schema)
      (Some(poemGlobalVariable), poemFunctions)
    } else (None, Vector.empty)
    val poemPropertyDefaultValueFunctions = generatePropertyDefaultValueFunctions(schema)

    (poemSchema, poemConstructor +: (poemObjectFunctions ++ poemPropertyDefaultValueFunctions), poemObject)
  }

  private def generateProperties(properties: Vector[StructPropertyDefinition]): Vector[PoemStructProperty] = {
    PropertyOrder.sort(properties.zipWithIndex)(_._1.name).map { case (property, declarationIndex) =>
      PoemStructProperty(property.name, TypeAssembler.generate(property.tpe), property.isOpen, declarationIndex)
    }
  }

  /**
    * Generates the constructor for the given schema. The constructor's parameters are ordered in their order of
    * declaration, while the `Struct` instruction expects the property values in their name order. Hence, the
    * constructor function has to translate between these two orders.
    *
    * TODO: If the resulting instructions are a simple `Struct` or `StructPoly`, we can inline the constructor in the
    *       ExpressionAssembler, when it's used directly. This is true if the struct has no open type parameters.
    */
  private def generateConstructor(schema: StructSchema)(implicit registry: Registry): PoemFunction = {
    implicit val registerProvider: RegisterProvider = new RegisterProvider

    // The first N registers are reserved for the property arguments in their declaration order.
    val propertyArgumentRegisters = schema.properties.map(property => property -> registerProvider.fresh()).toMap

    val regInstance = registerProvider.fresh()
    val orderedProperties = PropertyOrder.sort(schema.properties)(_.name)
    val valueArguments = orderedProperties.map(property => propertyArgumentRegisters(property))
    val bodyChunk = if (schema.isConstant) {
      val structType = TypeAssembler.generate(schema.constantType)
      Chunk(regInstance, PoemInstruction.Struct(regInstance, structType, valueArguments))
    } else {
      // For each type parameter, we either have to load the constructor function's argument type, or if the type
      // parameter is open, get its type via a type path from the actual value type.
      val typeArgumentChunks = schema.parameters.map { typeParameter =>
        if (!typeParameter.isOpen) {
          val regType = registerProvider.fresh()
          Chunk(regType, PoemInstruction.TypeArg(regType, typeParameter.index))
        } else {
          val regType = registerProvider.fresh()
          val property = schema.openParameterDerivations(typeParameter)
          val regProperty = propertyArgumentRegisters(property)
          val typePath = TypePath.of(property.tpe, typeParameter) match {
            case Vector(path) => path
            case _ => throw CompilationException(s"The type path to the open type parameter $typeParameter must exist and be unique. Schema: $schema.")
          }
          Chunk(PoemInstruction.TypeOf(regType, regProperty)) ++ TypePathAssembler.generate(regType, typePath)
        }
      }
      val typeArguments = typeArgumentChunks.map(_.forceResult(schema.position))
      val instanceChunk = Chunk(regInstance, PoemInstruction.StructPoly(regInstance, schema, typeArguments, valueArguments))
      Chunk.concat(typeArgumentChunks) ++ instanceChunk
    }

    val signature = schema.constructorSignature.copy(name = RuntimeNames.struct.constructor(schema))
    FunctionAssembler.generate(signature, Some(bodyChunk))
  }

  /**
    * Generates the poem global variable for an object given the schema.
    *
    * Objects without properties are generated as eager global variables and all other objects as lazy global
    * variables. This allows us to utilize the schema's constructor to build the object. Some objects might only have
    * constant default values, though, and could be initialized as eager global variables in the future.
    */
  private def generateObject(schema: StructSchema)(implicit registry: Registry): (PoemGlobalVariable, Vector[PoemFunction]) = {
    val name = RuntimeNames.struct.`object`(schema)

    if (schema.properties.isEmpty) {
      // Remember that objects cannot have type parameters.
      val poemType = PoemNamedType(schema, Vector.empty)
      val poemValue = PoemStructValue(Map.empty, poemType)
      (PoemEagerGlobalVariable(name, poemValue), Vector.empty)
    } else {
      implicit val registerProvider: RegisterProvider = new RegisterProvider

      // Remember that all properties of an object must have a default value.
      val propertyChunks = schema.properties.map(ConstructorAssembler.generatePropertyDefault)
      val constructorCallChunk = ConstructorAssembler.generateCall(schema.constantType, propertyChunks.map(_.forceResult))
      val bodyChunk = Chunk.concat(propertyChunks) ++ constructorCallChunk
      GlobalVariableAssembler.generateLazyGlobalVariable(name, schema.constantType, Right(bodyChunk), schema.position)
    }
  }

  /**
    * Generates the poem functions that compute default property values.
    */
  private def generatePropertyDefaultValueFunctions(schema: StructSchema)(implicit registry: Registry): Vector[PoemFunction] = {
    schema.properties.flatMap { property =>
      property.defaultValue.value match {
        case Some(defaultValue) =>
          val functionName = RuntimeNames.struct.defaultPropertyValue(property)
          val signature = FunctionSignature.constant(functionName, defaultValue.tpe, defaultValue.position)
          FunctionAssembler.generate(signature, Some(defaultValue), Map.empty)

        case None => Vector.empty
      }
    }
  }

}
