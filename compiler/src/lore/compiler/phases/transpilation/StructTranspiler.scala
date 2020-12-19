package lore.compiler.phases.transpilation

import lore.compiler.core.{Compilation, CompilationException}
import lore.compiler.core.Compilation.ToCompilationExtension
import lore.compiler.semantics.Registry
import lore.compiler.semantics.structures.PropertyDefinition
import lore.compiler.types.StructType

object StructTranspiler {

  /**
    * Transpiles a struct type and definition to its Javascript representation.
    *
    * A transpiled struct consists of the following parts:
    *   1. The type schema which saves the information (names, ownedBy, etc.) common to all instances of the struct
    *      type, regardless of which component types are actually instantiated.
    *   2. The "newtype" function which can be used to create a new instance of the struct type. This is needed
    *      because struct types are dependent on their actual components' types at run-time. Hence, for each struct
    *      instantiated, we also have to create a new type.
    *   3. The archetypal compile-time type instance, which is a struct type instantiated with the component types
    *      that it owns at compile-time. This type is used when being referred to in multiple dispatch and other
    *      static uses.
    *   4. An instantiation function which takes a members object and calculates the correct run-time type.
    *   5. One function for each default value of the struct's members. This function can be invoked by the code
    *      to generate another default value during instantiation.
    */
  def transpile(tpe: StructType)(implicit registry: Registry): Compilation[String] = {
    val (varSchema, schema) = transpileSchema(tpe)
    val (varNewtype, varArchetype, definitions) = transpileTypeDefinitions(tpe, varSchema)
    val instantiationFunction = transpileInstatiation(tpe, varNewtype, varArchetype)

    transpileDefaultValues(tpe).map { defaultValueFunctions =>
      s"""$schema
         |$definitions
         |$instantiationFunction
         |${defaultValueFunctions.mkString("\n")}
         |""".stripMargin
    }
  }

  private def transpileSchema(tpe: StructType) = {
    def transpileMemberDefinition(member: PropertyDefinition): String = s"{ name: '${member.name}' }"
    val propertyDefinitions = tpe.definition.properties.map(transpileMemberDefinition)
    val componentDefinitions = tpe.definition.components.map(transpileMemberDefinition)
    DeclaredTypeTranspiler.transpileSchema(
      tpe,
      RuntimeApi.types.schema.struct,
      Vector(
        s"[${propertyDefinitions.mkString(", ")}]",
        s"[${componentDefinitions.mkString(", ")}]",
      ),
    )
  }

  private def transpileTypeDefinitions(tpe: StructType, varSchema: TranspiledName) = {
    val varNewtype = TranspiledName.newType(tpe)
    val varArchetype = TranspiledName.declaredType(tpe)
    val definitions = if (tpe.isEntity) {
      val archetypeComponentTypes = tpe.componentSupertypes.map(RuntimeTypeTranspiler.transpile(_)(Map.empty))
      s"""function $varNewtype(componentTypes, isArchetype) {
         |  return ${RuntimeApi.types.struct}($varSchema, componentTypes, isArchetype);
         |}
         |const $varArchetype = $varNewtype([${archetypeComponentTypes.mkString(", ")}], true);""".stripMargin
    } else {
      s"""const $varArchetype = ${RuntimeApi.types.struct}($varSchema, [], true);"""
    }
    (varNewtype, varArchetype, definitions)
  }

  private def transpileInstatiation(tpe: StructType, varNewtype: TranspiledName, varArchetype: TranspiledName) = {
    val varInstantiate = TranspiledName.instantiate(tpe)
    val componentNames = tpe.definition.components.map(_.name)
    val instantiatedType = if (tpe.isEntity) {
      // Instantiates the object with the actual component types, which are retrieved using typeOf.
      s"$varNewtype([${componentNames.map { name =>
        s"${RuntimeApi.types.component}(${RuntimeApi.types.typeOf}(components.$name))"
      }.mkString(", ")}], false)"
    } else varArchetype

    s"""function $varInstantiate(members, components) {
       |  return ${RuntimeApi.values.`object`.create}(members, components, $instantiatedType);
       |}""".stripMargin
  }

  private def transpileDefaultValues(tpe: StructType)(implicit registry: Registry) = {
    tpe.definition.members.flatMap { member =>
      member.defaultValue.map { defaultValue =>
        val varDefaultValue = TranspiledName.defaultValue(tpe, member)

        // TODO: We need to supply runtime type variables here once structs can have type parameters.
        ExpressionTranspiler.transpile(defaultValue.expression)(registry, Map.empty).map { chunk =>
          if (chunk.expression.isEmpty) {
            throw CompilationException(s"A transpiled default value must always contain a chunk. Member ${member.name}" +
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
