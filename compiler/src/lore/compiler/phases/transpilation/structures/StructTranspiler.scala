package lore.compiler.phases.transpilation.structures

import lore.compiler.phases.transpilation.expressions.ExpressionTranspiler
import lore.compiler.phases.transpilation.{RuntimeApi, RuntimeNames, TypeTranspiler}
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
    *   5. A call-style constructor which delegates to the instantiation function. This constructor is essentially a
    *      function, which simplifies compilation and allows a user to pass struct constructors as functions.
    *   6. One function for each default value of the struct's properties. This function can be invoked to generate
    *      another default value during instantiation.
    */
  def transpile(tpe: StructType)(implicit registry: Registry): Vector[TargetStatement] = {
    val schema = transpileSchema(tpe)
    val (varNewtype, varArchetype, definitions) = transpileTypeDefinitions(tpe)
    val instantiate = transpileInstantiate(tpe, varNewtype, varArchetype)
    val constructor = transpileConstructor(tpe)
    val defaultValueFunctions = transpileDefaultValues(tpe)

    Vector(schema) ++ definitions ++ Vector(instantiate) ++ Vector(constructor) ++ defaultValueFunctions
  }

  private def transpileSchema(tpe: StructType) = {
    val varSchema = RuntimeNames.typeSchema(tpe)
    val propertyTypes = Target.Dictionary(tpe.definition.properties.map { property =>
      // As noted in the runtime's StructSchema definition, schema property types must be lazy to ensure that all
      // declared types are defined when the property type is initialized.
      Target.Property(property.name.asName, RuntimeApi.utils.`lazy`.of(TypeTranspiler.transpile(property.tpe)(Map.empty)))
    })

    varSchema.declareAs(
      RuntimeApi.structs.schema(
        tpe.name,
        DeclaredTypeTranspiler.transpileSupertraits(tpe),
        propertyTypes,
      ),
    )
  }

  private def transpileTypeDefinitions(tpe: StructType) = {
    val varSchema = RuntimeNames.typeSchema(tpe)
    val varNewtype = RuntimeNames.newType(tpe)
    val varArchetype = RuntimeNames.declaredType(tpe)
    val definitions = if (tpe.openProperties.nonEmpty) {
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
          Target.Return(RuntimeApi.structs.tpe(varSchema, paramIsArchetype.asVariable, paramPropertyTypes.asVariable)),
        )),
        varArchetype.declareAs(varNewtype.call(Target.BooleanLiteral(true))),
      )
    } else {
      Vector(
        varArchetype.declareAs(RuntimeApi.structs.tpe(varSchema, Target.BooleanLiteral(true), Target.Undefined)),
      )
    }
    (varNewtype, varArchetype, definitions)
  }

  private def transpileInstantiate(tpe: StructType, varNewtype: Target.Variable, varArchetype: Target.Variable) = {
    val varInstantiate = RuntimeNames.instantiate(tpe)
    val paramProperties = "properties".asParameter
    val instantiatedType = if (tpe.openProperties.nonEmpty) {
      // Instantiates the struct with the actual run-time property types, which are retrieved using typeOf. This
      // overrides the property types defined in the schema.
      val openPropertyTypes = Target.Dictionary(tpe.openProperties.map { property =>
        Target.Property(
          property.name.asName,
          RuntimeApi.types.typeOf(paramProperties.asVariable.prop(property.name)),
        )
      })
      varNewtype.call(Target.BooleanLiteral(false), openPropertyTypes)
    } else varArchetype

    Target.Function(varInstantiate.name, Vector(paramProperties), Target.block(
      Target.Return(RuntimeApi.structs.value(paramProperties.asVariable, instantiatedType)),
    ))
  }

  private def transpileConstructor(tpe: StructType) = {
    val varInstantiate = RuntimeNames.instantiate(tpe)
    val varConstructor = RuntimeNames.constructor(tpe)

    // We use the actual parameter names here so that we can construct the properties object using shorthand syntax.
    // For example:
    //    function ABC__constructor(name, age) {
    //      return ABC__instantiate({ name, age });
    //    }
    val parameterNames = tpe.definition.constructor.signature.parameters.map(_.name.asName)
    val parameters = parameterNames.map(Target.Parameter(_))
    val properties = Target.Dictionary(parameterNames.map(name => Target.Property(name, name.asVariable)))

    Target.Function(varConstructor.name, parameters, Target.block(
      Target.Return(Target.Call(varInstantiate, Vector(properties)))
    ))
  }

  private def transpileDefaultValues(tpe: StructType)(implicit registry: Registry) = {
    tpe.definition.properties.flatMap { property =>
      property.defaultValue.map { defaultValue =>
        val varDefaultValue = RuntimeNames.defaultValue(tpe, property)
        // TODO: We need to supply runtime type variables here once structs can have type parameters.
        val chunk = ExpressionTranspiler.transpile(defaultValue.expression)(registry, Map.empty)
        Target.Function(varDefaultValue.name, Vector.empty, chunk.asBody)
      }
    }
  }

}
