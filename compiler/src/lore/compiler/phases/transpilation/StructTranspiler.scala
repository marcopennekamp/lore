package lore.compiler.phases.transpilation

import lore.compiler.core.Compilation
import lore.compiler.core.Compilation.ToCompilationExtension
import lore.compiler.semantics.structures.MemberDefinition
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
    */
  def transpile(tpe: StructType): Compilation[String] = {
    def transpileMemberDefinition(member: MemberDefinition): String = s"{ name: '${member.name}' }"
    val propertyDefinitions = tpe.definition.properties.map(transpileMemberDefinition)
    val componentDefinitions = tpe.definition.components.map(transpileMemberDefinition)
    val (varSchema, schema) = DeclaredTypeTranspiler.transpileSchema(
      tpe,
      RuntimeApi.types.schema.struct,
      Vector(
        s"[${propertyDefinitions.mkString(", ")}]",
        s"[${componentDefinitions.mkString(", ")}]",
      ),
    )

    val varNewtype = TranspiledNames.newType(tpe)
    val varArchetype = TranspiledNames.declaredType(tpe)
    val definitions = if (tpe.isEntity) {
      val archetypeComponentTypes = tpe.componentSupertypes.map(RuntimeTypeTranspiler.transpile(_)(Map.empty))
      s"""function $varNewtype(componentTypes, isArchetype) {
         |  return ${RuntimeApi.types.struct}($varSchema, componentTypes, isArchetype);
         |}
         |const $varArchetype = $varNewtype([${archetypeComponentTypes.mkString(", ")}], true);""".stripMargin
    } else {
      s"""const $varArchetype = ${RuntimeApi.types.struct}($varSchema, [], true);"""
    }

    val varInstantiate = TranspiledNames.instantiate(tpe)
    val componentNames = tpe.definition.components.map(_.name)
    val instantiatedType = if (tpe.isEntity) {
      // Instantiates the object with the actual component types, which are retrieved using typeOf.
      s"$varNewtype([${componentNames.map { name =>
        s"${RuntimeApi.types.component}(${RuntimeApi.types.typeOf}(components.$name))"
      }.mkString(", ")}], false)"
    } else varArchetype
    val instantiationFunction = s"""function $varInstantiate(members, components) {
       |  return ${RuntimeApi.values.`object`.create}(members, components, $instantiatedType);
       |}""".stripMargin

    s"""$schema
       |$definitions
       |$instantiationFunction
       |""".stripMargin.compiled
  }

}
