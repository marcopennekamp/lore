package lore.compiler.resolution

import lore.compiler.feedback._
import lore.compiler.resolution.TypeDependencies.SchemaResolutionOrder
import lore.compiler.semantics.bindings.StructBinding
import lore.compiler.semantics.definitions.{TermDefinition, TypeDefinition}
import lore.compiler.semantics.functions.MultiFunctionDefinition
import lore.compiler.semantics.variables.GlobalVariableDefinition
import lore.compiler.semantics.{NamePath, Registry}
import lore.compiler.syntax.DeclNode
import lore.compiler.syntax.DeclNode.ModuleNode
import lore.compiler.types._
import lore.compiler.utils.CollectionExtensions.OptionExtension

object RegistryResolver {

  /**
    * Builds the registry from all module nodes.
    */
  def resolve(moduleNodes: Vector[DeclNode.ModuleNode])(implicit reporter: Reporter): Registry = {
    implicit val registry: Registry = new Registry
    resolveModules(moduleNodes)
    val typeDefinitions = initializeTypes()
    resolveTerms()
    registry.declaredTypeHierarchy.assign(new DeclaredTypeHierarchy(typeDefinitions))
    registry.coreDefinitions.assign(CoreDefinitionsResolver.resolve())
    registry
  }

  /**
    * Resolve global modules and local modules. All global modules need to be resolved first, as local modules rely on
    * module members in global modules via local declarations and imports.
    */
  private def resolveModules(moduleNodes: Vector[ModuleNode])(implicit registry: Registry, reporter: Reporter): Unit = {
    resolvePredefinedTypes()
    moduleNodes.foreach(GlobalModuleResolver.resolve)
    moduleNodes.foreach(LocalModuleResolver.resolve)
  }

  /**
    * Adds all predefined types to the root global module.
    */
  private def resolvePredefinedTypes()(implicit registry: Registry): Unit = {
    Type.predefinedTypes.values.foreach {
      tpe => registry.rootModule.types.add(tpe)
    }
  }

  /**
    * Initializes all schemas and struct bindings.
    *
    * We perform two separate passes over type declarations: (1) Initialize types and (2) initialize struct properties.
    * This has the distinct advantage that we don't need to defer typings of struct properties with laziness.
    *
    * TODO (multi-import): Some types with cyclic inheritance might need to be removed from global and local modules.
    *                      We have to make sure that the compiler doesn't crash if some types aren't initialized!
    *                      Basically, these uninitialized types should not be fetched from a scope. The best way to
    *                      solve this is probably to literally remove these types from global and local modules.
    *                      The same applies to alias StructBindings which haven't been initialized due to the aliased
    *                      type not being a struct type.
    */
  private def initializeTypes()(implicit registry: Registry, reporter: Reporter): Iterable[TypeDefinition] = {
    val typeDefinitions = collectTypeDefinitions()
    val schemaResolutionOrder = TypeDependencies.resolve(typeDefinitions)
    initializeSchemas(schemaResolutionOrder)
    initializeStructProperties(schemaResolutionOrder)
    typeDefinitions.values
  }

  /**
    * Collects all user-defined [[TypeDefinition]]s from all global modules in the registry, excluding basic types. In
    * contrast to other declarations, type definitions have to be collected centrally so that a schema resolution order
    * and later a declared type hierarchy can be built.
    */
  private def collectTypeDefinitions()(implicit registry: Registry): Map[NamePath, TypeDefinition] = {
    var typeDefinitions = Map.empty[NamePath, TypeDefinition]
    registry.modules.foreach { globalModule =>
      globalModule.types.all.foreach {
        case _: BasicType =>
        case moduleMember: TypeDefinition => typeDefinitions += moduleMember.name -> moduleMember
        case _ =>
      }
    }
    typeDefinitions
  }

  /**
    * Initializes schemas and struct bindings in their schema resolution order.
    */
  private def initializeSchemas(schemaResolutionOrder: SchemaResolutionOrder)(
    implicit registry: Registry,
    reporter: Reporter,
  ): Unit = {
    schemaResolutionOrder.foreach { typeDefinition =>
      lazy val structBinding = typeDefinition.globalModule.terms.get(typeDefinition.simpleName).filterType[StructBinding]
      typeDefinition match {
        case schema: AliasSchema =>
          AliasSchemaResolver.initialize(schema)
          structBinding.foreach(AliasSchemaResolver.initializeStructBinding(schema, _))

        case schema: TraitSchema =>
          TraitSchemaResolver.initialize(schema)

        case schema: StructSchema =>
          StructSchemaResolver.initialize(schema)
          structBinding.foreach(StructSchemaResolver.initializeStructBinding(schema, _))
      }
    }
  }

  private def initializeStructProperties(schemaResolutionOrder: SchemaResolutionOrder)(
    implicit registry: Registry,
    reporter: Reporter,
  ): Unit = {
    schemaResolutionOrder.foreach {
      case schema: StructSchema => StructSchemaResolver.initializeProperties(schema)
      case _ =>
    }
  }

  /**
    * Terms can be initialized in one go as global variables and multi-functions have no declarative interdependency.
    *
    * Struct bindings have already been initialized during type initialization.
    */
  private def resolveTerms()(implicit registry: Registry, reporter: Reporter): Unit = {
    registry.modules.foreach(_.terms.all.foreach(resolveTerm))
  }

  private def resolveTerm(moduleMember: TermDefinition)(
    implicit registry: Registry,
    reporter: Reporter,
  ): Unit = {
    moduleMember match {
      case variable: GlobalVariableDefinition => GlobalVariableDefinitionResolver.initialize(variable)
      case mf: MultiFunctionDefinition => MultiFunctionDefinitionResolver.initialize(mf)
      case _ =>
        // Global modules and struct bindings don't need to be initialized further.
    }
  }

}
