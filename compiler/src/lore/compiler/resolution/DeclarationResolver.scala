package lore.compiler.resolution

import lore.compiler.core.CompilationException
import lore.compiler.feedback._
import lore.compiler.semantics.Registry.Bindings
import lore.compiler.semantics.scopes.{StructBinding, StructConstructorBinding, StructObjectBinding}
import lore.compiler.semantics.{NamePath, Registry}
import lore.compiler.syntax.{DeclNode, TypeDeclNode}
import lore.compiler.types._
import lore.compiler.utils.CollectionExtensions.{OptionExtension, VectorExtension}

object DeclarationResolver {

  type TypeDeclarations = Map[NamePath, TypeDeclNode]

  /**
    * Builds the registry from all module nodes.
    *
    * We perform two separate passes over type declarations: (1) Resolve types and (2) resolve type definitions. This
    * has the distinct advantage that we don't need to defer typings of parameters and members when resolving
    * definitions, as all declared types have been added to the registry by then.
    */
  def resolve(moduleNodes: Vector[DeclNode.ModuleNode])(implicit reporter: Reporter): Registry = {
    val (localModules, globalModuleIndex) = ModuleResolver.resolve(moduleNodes)
    val moduleDefinitions = globalModuleIndex.modules.map {
      module => module.name -> module
    }.toMap
    val bindings1: Registry.Bindings = Registry.Bindings(moduleDefinitions, Map.empty, Map.empty, Map.empty)

    val allDeclarations = localModules.flatMap(_.members)
    val typeDeclNodes = allDeclarations.filterType[TypeDeclNode]
    val globalVariableDeclarations = allDeclarations.filterType[DeclNode.GlobalVariableNode]
    val multiFunctionDeclarations = allDeclarations.filterType[DeclNode.FunctionNode].groupBy(_.fullName)

    val typeDeclarations = typeDeclNodes.foldLeft(Map.empty: TypeDeclarations) {
      case (typeDeclarations, declaration) => processTypeDeclaration(declaration, typeDeclarations)
    }
    val schemaResolutionOrder = TypeDependencies.resolve(typeDeclarations)

    val schemas = resolveSchemasInOrder(typeDeclarations, schemaResolutionOrder)(bindings1, reporter)
    val schemaDefinitions = resolveSchemaDefinitionsInOrder(typeDeclarations, schemaResolutionOrder, schemas)(bindings1, reporter)
    implicit val types: Registry.Types = Registry.Types(schemas, schemaDefinitions)

    val globalVariables = resolveGlobalVariables(globalVariableDeclarations)(types, bindings1, reporter)
    val bindings2 = bindings1.copy(globalVariables = globalVariables)

    val multiFunctions = resolveMultiFunctions(multiFunctionDeclarations)(types, bindings2, reporter)
    val bindings3 = bindings2.copy(multiFunctions = multiFunctions)

    val structBindings = resolveStructBindings()(types, bindings3, reporter)
    val bindings4 = bindings3.copy(structBindings = structBindings)

    verifyBindingsUnique(bindings4)

    val coreDefinitions = CoreDefinitionsResolver.resolve()(types, bindings4, reporter)

    Registry(types, bindings4, coreDefinitions, schemaResolutionOrder)
  }

  private def processTypeDeclaration(declaration: TypeDeclNode, declarations: TypeDeclarations)(implicit reporter: Reporter): TypeDeclarations = {
    if (isTypeNameTaken(declaration.fullName, declarations)) {
      reporter.error(TypeFeedback.AlreadyExists(declaration))
      declarations
    } else {
      declarations + (declaration.fullName -> declaration)
    }
  }

  private def isTypeNameTaken(name: NamePath, typeDeclarations: TypeDeclarations): Boolean = {
    typeDeclarations.contains(name) || Type.predefinedTypes.contains(name)
  }

  private def resolveSchemasInOrder(
    typeDeclarations: TypeDeclarations,
    schemaResolutionOrder: Registry.SchemaResolutionOrder,
  )(implicit bindings: Bindings, reporter: Reporter): Registry.Schemas = {
    schemaResolutionOrder.foldLeft(Type.predefinedTypes: Registry.Schemas) {
      case (schemas, name) =>
        implicit val types: Registry.Types = Registry.Types(schemas, Map.empty)
        val tpe = typeDeclarations(name) match {
          case aliasNode: DeclNode.AliasNode => AliasSchemaResolver.resolve(aliasNode)
          case traitNode: DeclNode.TraitNode => TraitSchemaResolver.resolve(traitNode)
          case structNode: DeclNode.StructNode => StructSchemaResolver.resolve(structNode)
        }
        schemas + (name -> tpe)
    }
  }

  /**
    * This function guarantees that definitions are resolved in the type resolution order.
    */
  private def resolveSchemaDefinitionsInOrder(
    typeDeclarations: TypeDeclarations,
    schemaResolutionOrder: Registry.SchemaResolutionOrder,
    schemas: Registry.Schemas,
  )(implicit bindings: Registry.Bindings, reporter: Reporter): Registry.SchemaDefinitions = {
    schemaResolutionOrder.foldLeft(Map.empty: Registry.SchemaDefinitions) {
      case (schemaDefinitions, name) =>
        implicit val types: Registry.Types = Registry.Types(schemas, schemaDefinitions)
        val definition = typeDeclarations(name) match {
          case aliasNode: DeclNode.AliasNode => AliasDefinitionResolver.resolve(aliasNode)
          case traitNode: DeclNode.TraitNode => TraitDefinitionResolver.resolve(traitNode)
          case structNode: DeclNode.StructNode => StructDefinitionResolver.resolve(structNode)
        }
        schemaDefinitions + (name -> definition)
    }
  }

  private def resolveGlobalVariables(
    globalVariableDeclarations: Vector[DeclNode.GlobalVariableNode],
  )(implicit types: Registry.Types, bindings: Registry.Bindings, reporter: Reporter): Registry.GlobalVariables = {
    globalVariableDeclarations
      .map(GlobalVariableDefinitionResolver.resolve)
      .groupBy(_.name)
      .map {
        case (name, Vector(variable)) => name -> variable
        case (name, variables) =>
          variables.foreach(variable => reporter.error(GlobalVariableFeedback.AlreadyExists(variable)))
          name -> variables.head
      }
  }

  private def resolveMultiFunctions(
    multiFunctionDeclarations: Map[NamePath, Vector[DeclNode.FunctionNode]],
  )(implicit types: Registry.Types, bindings: Registry.Bindings, reporter: Reporter): Registry.MultiFunctions = {
    multiFunctionDeclarations.map {
      case (name, nodes) => name -> MultiFunctionDefinitionResolver.resolve(nodes)
    }
  }

  /**
    * Resolves a struct binding for each struct schema and type alias representing a struct type.
    *
    * In case of type aliases representing a struct type, the struct binding will be able to instantiate the struct
    * with the correct struct type given the type alias's type parameters.
    */
  def resolveStructBindings()(implicit types: Registry.Types, bindings: Registry.Bindings, reporter: Reporter): Registry.StructBindings = {
    types.schemas.flatMap {
      case (name, schema: StructSchema) =>
        if (schema.definition.isObject) {
          Some(name -> StructObjectBinding(name, schema.constantType))
        } else {
          Some(name -> StructConstructorBinding(name, schema.parameters, schema.instantiate(schema.identityAssignments)))
        }

      case (name, schema: AliasSchema) if schema.definition.isStructAlias =>
        Some(schema.originalType).filterType[StructType].map { tpe =>
          if (tpe.schema.definition.isObject) {
            name -> StructObjectBinding(name, tpe)
          } else {
            name -> StructConstructorBinding(name, schema.parameters, tpe)
          }
        }

      case _ => None
    }
  }

  /**
    * This function ensures that modules, global variables, and multi-functions don't share names. We don't have to
    * remove these entities from the Registry because the shadowing order is well defined. A multi-function will have
    * priority over a global variable with the same name, while the latter will have priority over a module.
    */
  private def verifyBindingsUnique(bindings: Bindings)(implicit reporter: Reporter): Unit = {
    bindings.modules.values.foreach { module =>
      if (bindings.globalVariables.contains(module.name) || bindings.multiFunctions.contains(module.name)) {
        reporter.error(ModuleFeedback.NameTaken(module))
      }
    }

    bindings.globalVariables.values.foreach { variable =>
      if (bindings.modules.contains(variable.name) || bindings.multiFunctions.contains(variable.name)) {
        reporter.error(GlobalVariableFeedback.NameTaken(variable))
      }
    }

    bindings.multiFunctions.values.foreach { mf =>
      if (bindings.modules.contains(mf.name) || bindings.globalVariables.contains(mf.name)) {
        reporter.error(MultiFunctionFeedback.NameTaken(mf))
      }
    }
  }

}
