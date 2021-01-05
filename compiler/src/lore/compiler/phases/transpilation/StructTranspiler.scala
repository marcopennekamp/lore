package lore.compiler.phases.transpilation

import lore.compiler.core.{Compilation, CompilationException}
import lore.compiler.semantics.Registry
import lore.compiler.types.StructType

object StructTranspiler {

  /**
    * Transpiles a struct type and definition to its Javascript representation.
    *
    * A transpiled struct consists of the following parts:
    *   1. The type schema which saves the information (property names, etc.) common to all instances of the struct
    *      type, regardless of which property types are actually instantiated.
    *   2. The "newtype" function which can be used to create a new instance of the struct type. This is needed
    *      because struct types might be dependent on some actual property types at run-time. Hence in such cases,
    *      for each struct instantiated, we also have to create a new type.
    *   3. The archetypal compile-time type instance, which is a struct type instantiated with the property types
    *      that it has at compile-time. This type is used when being referred to in multiple dispatch and other
    *      static uses. It is also used for structs that don't have open property types.
    *   4. An instantiation function which takes a properties object, computes the correct run-time type, and
    *      instantiates a value of the struct.
    *   5. One function for each default value of the struct's properties. This function can be invoked to generate
    *      another default value during instantiation.
    */
  def transpile(tpe: StructType)(implicit registry: Registry): Compilation[String] = {
    val (varSchema, schema) = transpileSchema(tpe)
    val (varNewtype, varArchetype, definitions) = transpileTypeDefinitions(tpe, varSchema)
    val instantiationFunction = transpileInstantiation(tpe, varNewtype, varArchetype)

    transpileDefaultValues(tpe).map { defaultValueFunctions =>
      s"""$schema
         |$definitions
         |$instantiationFunction
         |${defaultValueFunctions.mkString("\n")}
         |""".stripMargin
    }
  }

  private def transpileSchema(tpe: StructType) = {
    val propertyTypes = tpe.definition.properties.map { property =>
      // As noted in the runtime's StructSchema definition, schema property types must be lazy to ensure that all
      // declared types are defined when the property type is initialized.
      s"${property.name}: ${RuntimeApi.utils.`lazy`.of}(() => ${RuntimeTypeTranspiler.transpile(property.tpe)(Map.empty)})"
    }

    DeclaredTypeTranspiler.transpileSchema(
      tpe,
      RuntimeApi.structs.schema,
      Vector(
        s"{ ${propertyTypes.mkString(", ")} }",
      ),
    )
  }

  private def transpileTypeDefinitions(tpe: StructType, varSchema: TranspiledName) = {
    val varNewtype = TranspiledName.newType(tpe)
    val varArchetype = TranspiledName.declaredType(tpe)
    val definitions = if (tpe.hasOpenProperties) {
      // The type-specific property types are undefined when creating the archetype so that we don't have to evaluate
      // these lazily. (Archetypes are created right away, eagerly, and may then run into type ordering issues since
      // declared types are referenced by Javascript variable.)
      // This is legal because all of the archetype's "actual" property types are equal to its compile-time types,
      // so we don't need to override any property types. The schema will provide all these types when used in
      // subtyping.
      s"""function $varNewtype(isArchetype, propertyTypes) {
         |  return ${RuntimeApi.structs.tpe}($varSchema, isArchetype, propertyTypes);
         |}
         |const $varArchetype = $varNewtype(true);""".stripMargin
    } else {
      s"""const $varArchetype = ${RuntimeApi.structs.tpe}($varSchema, true);"""
    }
    (varNewtype, varArchetype, definitions)
  }

  private def transpileInstantiation(tpe: StructType, varNewtype: TranspiledName, varArchetype: TranspiledName) = {
    val varInstantiate = TranspiledName.instantiate(tpe)
    val instantiatedType = if (tpe.hasOpenProperties) {
      // TODO: We could only add types to the map that actually deviate from the schema's property types. However,
      //       such an "optimization" requires checking which might perform worse than the naive solution.
      // Instantiates the struct with the actual run-time property types, which are retrieved using typeOf. This
      // overrides the property types defined in the schema.
      val openPropertyTypes = tpe.openProperties.map { property =>
        s"${property.name}: ${RuntimeApi.types.typeOf}(properties.${property.name})"
      }
      s"$varNewtype(false, { ${openPropertyTypes.mkString(", ")} })"
    } else varArchetype

    s"""function $varInstantiate(properties) {
       |  return ${RuntimeApi.structs.value}(properties, $instantiatedType);
       |}""".stripMargin
  }

  private def transpileDefaultValues(tpe: StructType)(implicit registry: Registry) = {
    tpe.definition.properties.flatMap { property =>
      property.defaultValue.map { defaultValue =>
        val varDefaultValue = TranspiledName.defaultValue(tpe, property)

        // TODO: We need to supply runtime type variables here once structs can have type parameters.
        ExpressionTranspiler.transpile(defaultValue.expression)(registry, Map.empty).map { chunk =>
          if (chunk.expression.isEmpty) {
            throw CompilationException(s"A transpiled default value must always contain a chunk. Property ${property.name}" +
              s" of struct ${tpe.name} does not have such a valid default value.")
          }

          s"""function $varDefaultValue() {
             |  ${chunk.statements}
             |  return ${chunk.expression.get};
             |}""".stripMargin
        }
      }
    }.simultaneous
  }

}
