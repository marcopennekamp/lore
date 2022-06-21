package lore.compiler.resolution

import lore.compiler.feedback._
import lore.compiler.semantics.Registry.Terms
import lore.compiler.semantics.bindings.{StructConstructorBinding, StructObjectBinding}
import lore.compiler.semantics.{NamePath, Registry}
import lore.compiler.syntax.{DeclNode, TypeDeclNode}
import lore.compiler.types._
import lore.compiler.utils.CollectionExtensions.OptionExtension

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
    val (globalModuleIndex, declarationCollector) = ModuleResolver.resolve(moduleNodes)
    val moduleDefinitions = globalModuleIndex.modules.map {
      module => module.name -> module
    }.toMap
    val terms1: Registry.Terms = Registry.Terms(moduleDefinitions, Map.empty, Map.empty, Map.empty)

    val typeDeclarations = declarationCollector.typeDeclarations
    val schemaResolutionOrder = TypeDependencies.resolve(typeDeclarations)

    val schemas = resolveSchemasInOrder(typeDeclarations, schemaResolutionOrder)(terms1, reporter)
    val schemaDefinitions = resolveSchemaDefinitionsInOrder(typeDeclarations, schemaResolutionOrder, schemas)(terms1, reporter)
    implicit val types: Registry.Types = Registry.Types(schemas, schemaDefinitions)

    val globalVariables = resolveGlobalVariables(declarationCollector.globalVariableDeclarations)(types, terms1, reporter)
    val terms2 = terms1.copy(globalVariables = globalVariables)

    val multiFunctions = resolveMultiFunctions(declarationCollector.multiFunctionDeclarations)(types, terms2, reporter)
    val terms3 = terms2.copy(multiFunctions = multiFunctions)

    val structBindings = resolveStructBindings()(types, terms3, reporter)
    val terms4 = terms3.copy(structBindings = structBindings)

    val specs = resolveSpecs(declarationCollector.specDeclarations)(types, terms4, reporter)
    val coreDefinitions = CoreDefinitionsResolver.resolve()(types, terms4, reporter)
    Registry(types, terms4, specs, coreDefinitions, schemaResolutionOrder)
  }

  private def resolveSchemasInOrder(
    typeDeclarations: TypeDeclarations,
    schemaResolutionOrder: Registry.SchemaResolutionOrder,
  )(implicit terms: Terms, reporter: Reporter): Registry.Schemas = {
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
  )(implicit terms: Registry.Terms, reporter: Reporter): Registry.SchemaDefinitions = {
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
  )(implicit types: Registry.Types, terms: Registry.Terms, reporter: Reporter): Registry.GlobalVariables = {
    globalVariableDeclarations
      .map(GlobalVariableDefinitionResolver.resolve)
      .map(variable => variable.name -> variable)
      .toMap
  }

  private def resolveMultiFunctions(
    multiFunctionDeclarations: Map[NamePath, Vector[DeclNode.FunctionNode]],
  )(implicit types: Registry.Types, terms: Registry.Terms, reporter: Reporter): Registry.MultiFunctions = {
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
  def resolveStructBindings()(implicit types: Registry.Types, terms: Registry.Terms, reporter: Reporter): Registry.StructBindings = {
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

  private def resolveSpecs(
    specDeclarations: Vector[DeclNode.SpecNode],
  )(implicit types: Registry.Types, terms: Registry.Terms, reporter: Reporter): Registry.Specs = {
    specDeclarations.map(SpecDefinitionResolver.resolve)
  }

}
