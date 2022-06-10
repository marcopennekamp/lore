package lore.compiler.semantics

import lore.compiler.semantics.bindings.StructBinding
import lore.compiler.semantics.core.CoreDefinitions
import lore.compiler.semantics.functions.MultiFunctionDefinition
import lore.compiler.semantics.modules.{GlobalModule, LocalModule}
import lore.compiler.semantics.scopes._
import lore.compiler.semantics.specs.SpecDefinition
import lore.compiler.semantics.structures.SchemaDefinition
import lore.compiler.semantics.variables.GlobalVariableDefinition
import lore.compiler.types.{AliasSchema, DeclaredSchema, DeclaredTypeHierarchy, NamedSchema}
import lore.compiler.utils.CollectionExtensions.VectorExtension

/**
  * The Registry represents the global scope of module, type, global variable, and function definitions.
  */
case class Registry(
  types: Registry.Types,
  terms: Registry.Terms,
  specs: Registry.Specs,
  core: CoreDefinitions,
  schemaResolutionOrder: Registry.SchemaResolutionOrder,
) {

  val declaredTypeHierarchy = new DeclaredTypeHierarchy(types.schemas.values.toVector.filterType[DeclaredSchema])

  /**
    * All schemas in their proper order of resolution. Excludes predefined types.
    */
  val schemasInOrder: Vector[NamedSchema] = schemaResolutionOrder.map(name => types.schemas(name))

  /**
    * All schema definitions in their proper order of resolution.
    */
  lazy val schemaDefinitionsInOrder: Vector[SchemaDefinition] = schemasInOrder.map {
    case schema: DeclaredSchema => schema.definition
    case schema: AliasSchema => schema.definition
  }

  /**
    * An iterator that iterates through all schemas in order, then global variables, multi-functions, and specs.
    */
  def definitionsIterator: Iterator[Definition] = {
    schemaDefinitionsInOrder.iterator ++
      terms.globalVariables.valuesIterator ++
      terms.multiFunctions.valuesIterator ++
      specs.iterator
  }

  /**
    * Creates a type scope that represents the Registry. Name resolution requires the presence of a local module.
    */
  def getTypeScope(localModule: LocalModule): LocalModuleTypeScope = types.scope(localModule)

  /**
    * Creates a term scope that represents the Registry. Name resolution requires the presence of a local module.
    */
  def getTermScope(localModule: LocalModule): LocalModuleTermScope = terms.scope(localModule)

}

object Registry {
  type Schemas = Map[NamePath, NamedSchema]
  type SchemaDefinitions = Map[NamePath, SchemaDefinition]

  case class Types(
    schemas: Schemas,
    schemaDefinitions: SchemaDefinitions,
  ) {
    def scope(localModule: LocalModule): LocalModuleTypeScope = LocalModuleTypeScope(localModule, this)
  }

  type Modules = Map[NamePath, GlobalModule]
  type GlobalVariables = Map[NamePath, GlobalVariableDefinition]
  type MultiFunctions = Map[NamePath, MultiFunctionDefinition]
  type StructBindings = Map[NamePath, StructBinding]

  case class Terms(
    modules: Modules,
    globalVariables: GlobalVariables,
    multiFunctions: MultiFunctions,
    structBindings: StructBindings,
  ) {
    def scope(localModule: LocalModule): LocalModuleTermScope = LocalModuleTermScope(localModule, this)
  }

  type Specs = Vector[SpecDefinition]
  type SchemaResolutionOrder = Vector[NamePath]
}
