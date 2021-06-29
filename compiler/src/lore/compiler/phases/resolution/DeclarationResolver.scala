package lore.compiler.phases.resolution

import lore.compiler.core.Compilation.{FoldCompilationsExtension, ToCompilationExtension}
import lore.compiler.core.{Compilation, Position}
import lore.compiler.feedback.Feedback
import lore.compiler.semantics.scopes.{ImmutableTypeScope, TypeScope}
import lore.compiler.semantics.{Introspection, Registry}
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
  def resolve(declarations: Vector[DeclNode]): Compilation[Registry] = {
    val typeDeclNodes = introspectionTypeDeclarations ++ declarations.filterType[TypeDeclNode]
    val multiFunctionDeclarations = declarations.filterType[DeclNode.FunctionNode].groupBy(_.name)

    for {
      typeDeclarations <- typeDeclNodes.foldSimultaneous(Map.empty: TypeDeclarations) {
        case (typeDeclarations, declaration) => processTypeDeclaration(declaration, typeDeclarations)
      }
      typeResolutionOrder <- TypeDependencies.resolve(typeDeclarations)

      types <- resolveTypesInOrder(typeDeclarations, typeResolutionOrder)
      typeScope = ImmutableTypeScope(types, None)

      typeDefinitions <- resolveTypeDefinitionsInOrder(typeDeclarations, typeResolutionOrder)(typeScope)
      multiFunctions <- resolveMultiFunctions(multiFunctionDeclarations)(typeScope)
    } yield Registry(types, typeResolutionOrder, typeDefinitions, multiFunctions)
  }

  /**
    * The run-time Introspection API requires the compiler to generate a special "Type" trait that represents actual
    * Lore types. The trait cannot be defined in Pyramid because the compiler needs to call the initialization function
    * of the Introspection API with the actual type.
    *
    * TODO: We should refrain from keeping Pyramid optional and just add the trait to the core definitions. Then the
    *       compiler can just discover the trait and generate the correct API call.
    */
  private val introspectionTypeDeclarations: Vector[TypeDeclNode] = Vector(
    TypeDeclNode.TraitNode(Introspection.typeName, Vector.empty, Position.internal)
  )

  case class TypeAlreadyExists(node: TypeDeclNode) extends Feedback.Error(node) {
    override def message = s"The type ${node.name} is already declared somewhere else."
  }

  private def processTypeDeclaration(declaration: TypeDeclNode, declarations: TypeDeclarations): Compilation[TypeDeclarations] = {
    if (isTypeNameTaken(declaration.name, declarations)) {
      Compilation.fail(TypeAlreadyExists(declaration))
    } else {
      Compilation.succeed(declarations + (declaration.name -> declaration))
    }
  }

  private def isTypeNameTaken(name: String, typeDeclarations: TypeDeclarations): Boolean = {
    typeDeclarations.contains(name) || Type.predefinedTypes.contains(name)
  }

  private def resolveTypesInOrder(typeDeclarations: TypeDeclarations, typeResolutionOrder: Registry.TypeResolutionOrder): Compilation[Registry.Types] = {
    typeResolutionOrder.foldSimultaneous(Type.predefinedTypes: Registry.Types) {
      case (types, name) =>
        implicit val typeScope: TypeScope = ImmutableTypeScope(types, None)
        val compilation = typeDeclarations(name) match {
          case aliasNode: TypeDeclNode.AliasNode => AliasTypeResolver.resolve(aliasNode)
          case traitNode: TypeDeclNode.TraitNode => TraitTypeResolver.resolve(traitNode)
          case structNode: TypeDeclNode.StructNode => StructTypeResolver.resolve(structNode)
        }
        compilation.map(tpe => types + (name -> tpe))
    }
  }

  /**
    * This function guarantees that definitions are returned in the type resolution order.
    */
  private def resolveTypeDefinitionsInOrder(
    typeDeclarations: TypeDeclarations,
    typeResolutionOrder: Registry.TypeResolutionOrder,
  )(implicit typeScope: TypeScope): Compilation[Registry.TypeDefinitions] = {
    typeResolutionOrder.foldSimultaneous(Map.empty: Registry.TypeDefinitions) {
      case (typeDefinitions, name) =>
        typeDeclarations(name) match {
          case _: TypeDeclNode.AliasNode => typeDefinitions.compiled

          case traitNode: TypeDeclNode.TraitNode =>
            TraitDefinitionResolver
              .resolve(traitNode)
              .map(definition => typeDefinitions + (name -> definition))

          case structNode: TypeDeclNode.StructNode =>
            StructDefinitionResolver
              .resolve(structNode)
              .map(definition => typeDefinitions + (name -> definition))
        }
    }
  }

  private def resolveMultiFunctions(
    multiFunctionDeclarations: Map[String, Vector[DeclNode.FunctionNode]],
  )(implicit typeScope: TypeScope): Compilation[Registry.MultiFunctions] = {
    multiFunctionDeclarations.map {
      case (name, nodes) => MultiFunctionDefinitionResolver.resolve(nodes).map(name -> _)
    }.toVector.simultaneous.map(_.toMap)
  }

}
