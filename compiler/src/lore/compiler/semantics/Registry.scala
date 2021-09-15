package lore.compiler.semantics

import lore.compiler.semantics.functions.MultiFunctionDefinition
import lore.compiler.semantics.modules.{LocalModule, ModuleDefinition}
import lore.compiler.semantics.scopes._
import lore.compiler.semantics.structures.SchemaDefinition
import lore.compiler.semantics.variables.GlobalVariableDefinition
import lore.compiler.types.{DeclaredSchema, DeclaredTypeHierarchy, NamedSchema}
import lore.compiler.utils.CollectionExtensions.VectorExtension

/**
  * The Registry represents the global scope of module, type, global variable, and function definitions.
  */
case class Registry(
  types: Registry.Types,
  bindings: Registry.Bindings,
  schemaResolutionOrder: Registry.SchemaResolutionOrder,
) {

  val declaredTypeHierarchy = new DeclaredTypeHierarchy(types.schemas.values.toVector.filterType[DeclaredSchema])

  /**
    * All schemas in their proper order of resolution. Excludes predefined types.
    */
  val schemasInOrder: Vector[(NamePath, NamedSchema)] = schemaResolutionOrder.map(name => (name, types.schemas(name)))

  /**
    * Creates a type scope that represents the Registry. Name resolution requires the presence of a local module.
    */
  def getTypeScope(localModule: LocalModule): LocalModuleTypeScope = LocalModuleTypeScope(localModule, types)

  /**
    * Creates a binding scope that represents the Registry. Name resolution requires the presence of a local module.
    */
  def getBindingScope(localModule: LocalModule): LocalModuleBindingScope = LocalModuleBindingScope(localModule, bindings)

}

object Registry {
  type Schemas = Map[NamePath, NamedSchema]
  type SchemaDefinitions = Map[NamePath, SchemaDefinition]

  case class Types(
    schemas: Schemas,
    schemaDefinitions: SchemaDefinitions,
  )

  type GlobalVariables = Map[NamePath, GlobalVariableDefinition]
  type MultiFunctions = Map[NamePath, MultiFunctionDefinition]
  type StructBindings = Map[NamePath, StructBinding]
  type Modules = Map[NamePath, ModuleDefinition]

  case class Bindings(
    globalVariables: GlobalVariables,
    multiFunctions: MultiFunctions,
    structBindings: StructBindings,
    modules: Modules,
  )

  type SchemaResolutionOrder = Vector[NamePath]
}
