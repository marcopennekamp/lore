package lore.compiler.resolution

import lore.compiler.feedback._
import lore.compiler.resolution.TypeDependencies.SchemaInitializationOrder
import lore.compiler.semantics.definitions.{TermDefinition, TypeDefinition}
import lore.compiler.semantics.functions.MultiFunctionDefinition
import lore.compiler.semantics.variables.GlobalVariableDefinition
import lore.compiler.semantics.{NamePath, Registry}
import lore.compiler.syntax.DeclNode
import lore.compiler.syntax.DeclNode.ModuleNode
import lore.compiler.types._

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
    * Cyclic type definitions are fallback-initialized. See [[initializeCyclicSchemas]]. This happens before the schema
    * initialization order is iterated so that valid types which rely on these cyclic types can correctly reference
    * them.
    *
    * Struct alias bindings which don't have an underlying struct type are fallback-initialized with a mock struct or
    * object.
    */
  private def initializeTypes()(implicit registry: Registry, reporter: Reporter): Iterable[TypeDefinition] = {
    val typeDefinitions = collectTypeDefinitions()
    val (schemaInitializationOrder, cyclicTypeDefinitions) = TypeDependencies.resolve(typeDefinitions)
    initializeCyclicSchemas(cyclicTypeDefinitions)
    initializeSchemas(schemaInitializationOrder)

    // We need to initialize the struct properties of both cyclic types and regularly initialized types. Property
    // initialization of cyclic types does not differ from regular types.
    initializeStructProperties(cyclicTypeDefinitions ++ schemaInitializationOrder)

    typeDefinitions.values
  }

  /**
    * Collects all user-defined [[TypeDefinition]]s from all global modules in the registry, excluding basic types. In
    * contrast to other declarations, type definitions have to be collected centrally so that a schema initialization
    * order and later a declared type hierarchy can be built.
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
    * Initializes schemas and struct bindings which haven't been added to the schema initialization order due to cyclic
    * dependencies. They are fallback-initialized specially as "empty" types (without type parameter bounds and
    * extended types) so that the compiler can continue to work with these types. Mainly, we want to avoid the compiler
    * crashing due to an uninitialized type.
    */
  private def initializeCyclicSchemas(typeDefinitions: Vector[TypeDefinition])(
    implicit registry: Registry,
    reporter: Reporter,
  ): Unit = {
    typeDefinitions.foreach {
      case schema: AliasSchema =>
        AliasSchemaResolver.fallbackInitialize(schema)
        schema.structBinding.foreach(AliasSchemaResolver.initializeStructBinding(schema, _))

      case schema: TraitSchema =>
        DeclaredSchemaResolver.fallbackInitialize(schema)

      case schema: StructSchema =>
        DeclaredSchemaResolver.fallbackInitialize(schema)
        schema.structBinding.foreach(StructSchemaResolver.initializeStructBinding(schema, _))
    }
  }

  /**
    * Initializes schemas and struct bindings in their schema initialization order.
    */
  private def initializeSchemas(schemaInitializationOrder: SchemaInitializationOrder)(
    implicit registry: Registry,
    reporter: Reporter,
  ): Unit = {
    schemaInitializationOrder.foreach {
      case schema: AliasSchema =>
        AliasSchemaResolver.initialize(schema)
        schema.structBinding.foreach(AliasSchemaResolver.initializeStructBinding(schema, _))

      case schema: TraitSchema =>
        DeclaredSchemaResolver.initialize(schema)

      case schema: StructSchema =>
        DeclaredSchemaResolver.initialize(schema)
        schema.structBinding.foreach(StructSchemaResolver.initializeStructBinding(schema, _))
    }
  }

  private def initializeStructProperties(schemaInitializationOrder: SchemaInitializationOrder)(
    implicit registry: Registry,
    reporter: Reporter,
  ): Unit = {
    schemaInitializationOrder.foreach {
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
