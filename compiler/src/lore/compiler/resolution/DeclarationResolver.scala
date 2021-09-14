package lore.compiler.resolution

import lore.compiler.core.Position
import lore.compiler.feedback.{GlobalVariableFeedback, MultiFunctionFeedback, Reporter, TypeFeedback}
import lore.compiler.semantics.scopes.{ImmutableTypeScope, TypeScope}
import lore.compiler.semantics.{Introspection, Registry}
import lore.compiler.syntax.Node.NameNode
import lore.compiler.syntax.{DeclNode, TypeDeclNode}
import lore.compiler.types.Type
import lore.compiler.utils.CollectionExtensions.VectorExtension

object DeclarationResolver {

  type TypeDeclarations = Map[String, TypeDeclNode]

  /**
    * Builds the registry from all declarations.
    *
    * We perform two separate passes over type declarations: (1) Resolve types and (2) resolve type definitions. This
    * has the distinct advantage that we don't need to defer typings of parameters and members when resolving
    * definitions, as all declared types have been added to the registry by then.
    */
  def resolve(declarations: Vector[DeclNode])(implicit reporter: Reporter): Registry = {
    val typeDeclNodes = introspectionTypeDeclarations ++ declarations.filterType[TypeDeclNode]
    val globalVariableDeclarations = declarations.filterType[DeclNode.GlobalVariableNode]
    val multiFunctionDeclarations = declarations.filterType[DeclNode.FunctionNode].groupBy(_.name)

    val typeDeclarations = typeDeclNodes.foldLeft(Map.empty: TypeDeclarations) {
      case (typeDeclarations, declaration) => processTypeDeclaration(declaration, typeDeclarations)
    }
    val schemaResolutionOrder = TypeDependencies.resolve(typeDeclarations)

    val types = resolveSchemasInOrder(typeDeclarations, schemaResolutionOrder)
    implicit val typeScope: TypeScope = ImmutableTypeScope(types, None)

    val schemaDefinitions = resolveSchemaDefinitionsInOrder(typeDeclarations, schemaResolutionOrder)
    val globalVariables = resolveGlobalVariables(globalVariableDeclarations)
    val multiFunctions = resolveMultiFunctions(multiFunctionDeclarations, globalVariables)

    Registry(types, schemaResolutionOrder, schemaDefinitions, globalVariables, multiFunctions)
  }

  /**
    * The run-time Introspection API requires the compiler to generate a special "Type" trait that represents actual
    * Lore types. The trait cannot be defined in Pyramid because the compiler needs to call the initialization function
    * of the Introspection API with the actual type.
    *
    * TODO (modules): Just define this in `lore.Core` and have the compiler get the trait from the module.
    */
  private val introspectionTypeDeclarations: Vector[TypeDeclNode] = Vector(
    DeclNode.TraitNode(NameNode(Introspection.typeName, Position.internal), Vector.empty, Vector.empty, Position.internal)
  )

  private def processTypeDeclaration(declaration: TypeDeclNode, declarations: TypeDeclarations)(implicit reporter: Reporter): TypeDeclarations = {
    if (isTypeNameTaken(declaration.name, declarations)) {
      reporter.error(TypeFeedback.AlreadyExists(declaration))
      declarations
    } else {
      declarations + (declaration.name -> declaration)
    }
  }

  private def isTypeNameTaken(name: String, typeDeclarations: TypeDeclarations): Boolean = {
    typeDeclarations.contains(name) || Type.predefinedTypes.contains(name)
  }

  private def resolveSchemasInOrder(
    typeDeclarations: TypeDeclarations,
    schemaResolutionOrder: Registry.SchemaResolutionOrder,
  )(implicit reporter: Reporter): Registry.Schemas = {
    schemaResolutionOrder.foldLeft(Type.predefinedTypes: Registry.Schemas) {
      case (types, name) =>
        val typeScope: TypeScope = ImmutableTypeScope(types, None)
        val tpe = typeDeclarations(name) match {
          case aliasNode: DeclNode.AliasNode => AliasSchemaResolver.resolve(aliasNode, typeScope)
          case traitNode: DeclNode.TraitNode => TraitSchemaResolver.resolve(traitNode, typeScope)
          case structNode: DeclNode.StructNode => StructSchemaResolver.resolve(structNode, typeScope)
        }
        types + (name -> tpe)
    }
  }

  /**
    * This function guarantees that definitions are resolved in the type resolution order.
    */
  private def resolveSchemaDefinitionsInOrder(
    typeDeclarations: TypeDeclarations,
    schemaResolutionOrder: Registry.SchemaResolutionOrder,
  )(implicit typeScope: TypeScope, reporter: Reporter): Registry.SchemaDefinitions = {
    schemaResolutionOrder.foldLeft(Map.empty: Registry.SchemaDefinitions) {
      case (schemaDefinitions, name) =>
        val definition = typeDeclarations(name) match {
          case aliasNode: DeclNode.AliasNode => AliasDefinitionResolver.resolve(aliasNode, typeScope)
          case traitNode: DeclNode.TraitNode => TraitDefinitionResolver.resolve(traitNode, typeScope)
          case structNode: DeclNode.StructNode => StructDefinitionResolver.resolve(structNode, typeScope)
        }
        schemaDefinitions + (name -> definition)
    }
  }

  private def resolveGlobalVariables(
    globalVariableDeclarations: Vector[DeclNode.GlobalVariableNode],
  )(implicit typeScope: TypeScope, reporter: Reporter): Registry.GlobalVariables = {
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
    multiFunctionDeclarations: Map[String, Vector[DeclNode.FunctionNode]],
    globalVariables: Registry.GlobalVariables,
  )(implicit typeScope: TypeScope, reporter: Reporter): Registry.MultiFunctions = {
    val multiFunctions = multiFunctionDeclarations.map {
      case (name, nodes) => name -> MultiFunctionDefinitionResolver.resolve(nodes)
    }

    // Ensure that global variables and multi-functions don't share names. We don't have to remove these
    // multi-functions from the Registry because multi-functions are shadowed by global variables.
    multiFunctions.foreach { case (name, mf) =>
      globalVariables.get(name).foreach { variable =>
        reporter.error(MultiFunctionFeedback.NameTakenByVariable(mf, variable))
      }
    }

    multiFunctions
  }

}
