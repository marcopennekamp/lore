package lore.compiler.phases.transpilation

import lore.compiler.semantics.Registry
import lore.compiler.target.Target
import lore.compiler.target.Target.TargetStatement
import lore.compiler.target.TargetDsl._
import lore.compiler.types.StructType

object StructTranspiler {

  /**
    * Transpiles a struct type and definition to its target representation.
    *
    * A transpiled struct consists of the following parts:
    *   1. The type schema which saves the information (property names, etc.) common to all instances of the struct
    *      type, regardless of which property types are actually instantiated.
    *   2. The "newtype" function which can be used to create a new instance of the struct type. This is needed
    *      because struct types might be dependent on some actual property types at run-time. Hence in such cases,
    *      for each struct instantiated, we also have to create a new type.
    *   3. The archetypal compile-time type instance, which is a struct type instantiated with the property types
    *      that it has at compile-time. This type is used when being referred to as the supertype in multiple dispatch
    *      and other static uses. It is also used for structs that don't have open property types.
    *   4. An instantiation function which takes a properties object, computes the correct run-time type, and
    *      instantiates a value of the struct.
    *   5. One function for each default value of the struct's properties. This function can be invoked to generate
    *      another default value during instantiation.
    */
  def transpile(tpe: StructType)(implicit registry: Registry): Vector[TargetStatement] = {
    val schema = transpileSchema(tpe)
    val (varNewtype, varArchetype, definitions) = transpileTypeDefinitions(tpe)
    val instantiationFunction = transpileInstantiation(tpe, varNewtype, varArchetype)
    val defaultValueFunctions = transpileDefaultValues(tpe)

    Vector(schema) ++ definitions ++ Vector(instantiationFunction) ++ defaultValueFunctions
  }

  private def transpileSchema(tpe: StructType) = {
    val varSchema = TranspiledName.typeSchema(tpe).asVariable
    val propertyTypes = Target.Dictionary(tpe.definition.properties.map { property =>
      // As noted in the runtime's StructSchema definition, schema property types must be lazy to ensure that all
      // declared types are defined when the property type is initialized.
      Target.Property(property.name.asName, RuntimeApi.utils.`lazy`.of(RuntimeTypeTranspiler.transpile(property.tpe)(Map.empty)))
    })

    varSchema.declareAs(
      RuntimeApi.structs.schema(
        tpe.name,
        DeclaredTypeTranspiler.transpileSupertraits(tpe),
        propertyTypes
      )
    )
  }

  private def transpileTypeDefinitions(tpe: StructType) = {
    val varSchema = TranspiledName.typeSchema(tpe).asVariable
    val varNewtype = TranspiledName.newType(tpe).asVariable
    val varArchetype = TranspiledName.declaredType(tpe).asVariable
    val definitions = if (tpe.hasOpenProperties) {
      // The type-specific property types are undefined when creating the archetype so that we don't have to evaluate
      // these lazily. (Archetypes are created right away, eagerly, and may then run into type ordering issues since
      // declared types are referenced by Javascript variable.)
      // This is legal because all of the archetype's "actual" property types are equal to its compile-time types,
      // so we don't need to override any property types. The schema will provide all these types when used in
      // subtyping.
      val paramIsArchetype = "isArchetype".asParameter
      val paramPropertyTypes = "propertyTypes".asParameter
      Vector(
        Target.Function(varNewtype.name, Vector(paramIsArchetype, paramPropertyTypes), Target.block(
          Target.Return(RuntimeApi.structs.tpe(varSchema, paramIsArchetype.asVariable, paramPropertyTypes.asVariable))
        )),
        varArchetype.declareAs(varNewtype.call(Target.BooleanLiteral(true)))
      )
    } else {
      Vector(
        varArchetype.declareAs(RuntimeApi.structs.tpe(varSchema, Target.BooleanLiteral(true), Target.Undefined))
      )
    }
    (varNewtype, varArchetype, definitions)
  }

  private def transpileInstantiation(tpe: StructType, varNewtype: Target.Variable, varArchetype: Target.Variable) = {
    val varInstantiate = TranspiledName.instantiate(tpe).asVariable
    val paramProperties = "properties".asParameter
    val instantiatedType = if (tpe.hasOpenProperties) {
      // TODO: We could only add types to the map that actually deviate from the schema's property types. However,
      //       such an "optimization" requires checking which might perform worse than the naive solution.
      // Instantiates the struct with the actual run-time property types, which are retrieved using typeOf. This
      // overrides the property types defined in the schema.
      val openPropertyTypes = Target.Dictionary(tpe.openProperties.map { property =>
        Target.Property(
          property.name.asName,
          RuntimeApi.types.typeOf(paramProperties.asVariable.prop(property.name))
        )
      })
      varNewtype.call(Target.BooleanLiteral(false), openPropertyTypes)
    } else varArchetype

    Target.Function(varInstantiate.name, Vector(paramProperties), Target.block(
      Target.Return(RuntimeApi.structs.value(paramProperties.asVariable, instantiatedType))
    ))
  }

  private def transpileDefaultValues(tpe: StructType)(implicit registry: Registry) = {
    tpe.definition.properties.flatMap { property =>
      property.defaultValue.map { defaultValue =>
        val varDefaultValue = TranspiledName.defaultValue(tpe, property).asVariable
        // TODO: We need to supply runtime type variables here once structs can have type parameters.
        val chunk = ExpressionTranspiler.transpile(defaultValue.expression)(registry, Map.empty)
        Target.Function(varDefaultValue.name, Vector.empty, chunk.asBody)
      }
    }
  }

}
