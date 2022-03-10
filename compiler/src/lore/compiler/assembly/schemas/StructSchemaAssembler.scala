package lore.compiler.assembly.schemas

import lore.compiler.assembly.functions.FunctionAssembler
import lore.compiler.assembly.types.{TypeAssembler, TypePathAssembler}
import lore.compiler.assembly.{AsmChunk, AsmRuntimeNames, PropertyOrder, RegisterProvider}
import lore.compiler.core.CompilationException
import lore.compiler.poem._
import lore.compiler.semantics.Registry
import lore.compiler.semantics.structures.StructPropertyDefinition
import lore.compiler.types.{StructSchema, TypePath}

object StructSchemaAssembler {

  /**
    * Generates a PoemStructSchema for the given schema, any auxiliary functions the schema requires (including the
    * constructor), and a PoemGlobalVariable for the object.
    *
    * TODO (assembly): Support property default values.
    */
  def generate(
    schema: StructSchema,
    poemTypeParameters: Vector[PoemTypeParameter],
    poemSupertraits: Vector[PoemNamedType],
  )(implicit registry: Registry): (PoemStructSchema, Vector[PoemFunction], Option[PoemGlobalVariable]) = {
    val orderedProperties = PropertyOrder.sort(schema.definition.properties)(_.name)
    val poemProperties = orderedProperties.map(generateProperty)

    val poemSchema = PoemStructSchema(schema.kind, schema.name, poemTypeParameters, poemSupertraits, poemProperties)
    val poemConstructor = generateConstructor(schema, orderedProperties)
    val poemObject = if (schema.definition.isObject) Some(generateObject(schema)) else None

    if (schema.definition.properties.exists(_.hasDefault)) {
      throw CompilationException(s"Default values aren't supported yet. Schema: ${schema.name}.")
    }

    (poemSchema, Vector(poemConstructor), poemObject)
  }

  private def generateProperty(property: StructPropertyDefinition): PoemStructProperty = {
    PoemStructProperty(property.name, TypeAssembler.generate(property.tpe), property.isOpen)
  }

  /**
    * Generates the constructor for the given schema. The constructor's parameters are ordered in their order of
    * declaration, while the `Struct` instruction expects the property values in their name order. Hence, the
    * constructor function has to translate between these two orders.
    *
    * TODO (assembly): If the resulting instructions are a simple `Struct` or `StructPoly`, we can inline the
    *                  constructor in the ExpressionAssembler, when it's used directly. This is true if the struct has
    *                  no open type parameters.
    */
  private def generateConstructor(
    schema: StructSchema,
    orderedProperties: Vector[StructPropertyDefinition],
  )(implicit registry: Registry): PoemFunction = {
    implicit val registerProvider: RegisterProvider = new RegisterProvider

    // The first N registers are reserved for the property arguments in their declaration order.
    val propertyArgumentRegisters = schema.definition.properties.map(property => property -> registerProvider.fresh()).toMap

    val regInstance = registerProvider.fresh()
    val valueArguments = orderedProperties.map(property => propertyArgumentRegisters(property))
    val bodyChunk = if (schema.isConstant) {
      val structType = TypeAssembler.generate(schema.constantType)
      AsmChunk(regInstance, PoemInstruction.Struct(regInstance, structType, valueArguments))
    } else {
      // For each type parameter, we either have to load the constructor function's argument type, or if the type
      // parameter is open, get its type via a type path from the actual value type.
      val typeArgumentChunks = schema.parameters.map { typeParameter =>
        if (!typeParameter.isOpen) {
          val regType = registerProvider.fresh()
          AsmChunk(regType, PoemInstruction.TypeArg(regType, typeParameter.index))
        } else {
          val regType = registerProvider.fresh()
          val property = schema.openParameterDerivations(typeParameter)
          val regProperty = propertyArgumentRegisters(property)
          val typePath = TypePath.of(property.tpe, typeParameter) match {
            case Vector(path) => path
            case _ => throw CompilationException(s"The type path to the open type parameter $typeParameter must exist and be unique. Schema: $schema.")
          }
          AsmChunk(PoemInstruction.TypeOf(regType, regProperty)) ++ TypePathAssembler.generate(regType, typePath)
        }
      }
      val typeArguments = typeArgumentChunks.map(_.forceResult(schema.definition.position))
      val instanceChunk = AsmChunk(regInstance, PoemInstruction.StructPoly(regInstance, schema, typeArguments, valueArguments))
      AsmChunk.concat(typeArgumentChunks) ++ instanceChunk
    }

    val signature = schema.constructorSignature.copy(name = AsmRuntimeNames.struct.construct(schema))
    FunctionAssembler.generate(signature, Some(bodyChunk))
  }

  /**
    * Generates the poem global variable for an object given the schema.
    *
    * Objects without properties are generated as eager global variables and all other objects as lazy global
    * variables. This allows us to utilize the schema's constructor to build the object. Some objects might only have
    * constant default values, though, and could be initialized as eager global variables in the future.
    */
  private def generateObject(schema: StructSchema): PoemGlobalVariable = {
    val name = AsmRuntimeNames.struct.`object`(schema)

    if (schema.definition.properties.isEmpty) {
      // Remember that objects cannot have type parameters.
      val poemType = PoemNamedType(schema, Vector.empty)
      val poemValue = PoemStructValue(Map.empty, poemType)
      PoemEagerGlobalVariable(name, poemValue)
    } else {
      throw CompilationException("Objects with properties aren't supported yet.")
      // Remember that all properties of an object must have a default value.
//      schema.definition.properties.map {
//        property => property.defaultValue.get
//      }
    }
  }

}
