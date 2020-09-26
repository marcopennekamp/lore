package lore.compiler.phases.transpilation

import lore.compiler.core.Compilation
import lore.compiler.core.Compilation.ToCompilationExtension
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
    // TODO: Transpile member definitions into the schema?
    val (varSchema, schema) = DeclaredTypeTranspiler.transpileSchema(tpe, RuntimeApi.types.schema.struct)

    val varNewtype = TranspiledNames.newType(tpe)
    val varArchetype = TranspiledNames.declaredType(tpe)
    val definitions = if (tpe.isEntity) {
      val archetypeComponentTypes = tpe.componentSupertypes.map(RuntimeTypeTranspiler.transpile(_)(Map.empty))
      s"""function $varNewtype(componentTypes) {
         |  return ${RuntimeApi.types.struct}($varSchema, componentTypes);
         |}
         |const $varArchetype = $varNewtype([${archetypeComponentTypes.mkString(", ")}]);""".stripMargin
    } else {
      s"""const $varArchetype = ${RuntimeApi.types.struct}($varSchema, []);"""
    }

    val varInstantiate = TranspiledNames.instantiate(tpe)
    val componentNames = tpe.definition.components.map(_.name)
    val instantiatedType = if (tpe.isEntity) {
      // Instantiates the object with the actual component types, which are retrieved using typeOf.
      s"$varNewtype([${componentNames.map { name =>
        s"${RuntimeApi.types.component}(${RuntimeApi.types.typeOf}(members.$name))"
      }.mkString(", ")}])"
    } else varArchetype
    val instantiationFunction = s"""function $varInstantiate(members) {
       |  return ${RuntimeApi.values.`object`.create}(members, $instantiatedType);
       |}""".stripMargin

    s"""$schema
       |$definitions
       |$instantiationFunction
       |""".stripMargin.compiled
  }

}
