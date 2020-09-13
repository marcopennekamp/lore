package lore.compiler.phases.resolution

import lore.compiler.core.Compilation.Verification
import lore.compiler.core.{Compilation, CompilationException, Error}
import lore.compiler.phases.resolution.DeclarationResolver.TypeAlreadyExists
import lore.compiler.semantics.functions.FunctionDefinition
import lore.compiler.semantics.{Registry, TypeScope}
import lore.compiler.syntax.{DeclNode, TypeDeclNode}
import lore.compiler.types.Type
import lore.compiler.utils.CollectionExtensions.FilterTypeVectorExtension

import scala.collection.mutable

/**
  * Declarations can occur unordered. This is not only true within a file, but especially across multiple files. We
  * can declare class hierarchies across Lore files, define functions on their own, and so on. The way in which
  * types and definitions are represented within the compiler requires us to find an order in which we can properly
  * resolve these types, especially classes and entities.
  *
  * The DeclarationResolver holds all top-level declarations across all files parsed by the Lore compiler
  * still in AST form. While files are being parsed, it builds a hierarchy of types, which gives us the correct order
  * in which we should, for example, build ClassDefinition and ClassType objects. Once all type declarations have been
  * turned into definitions, the DeclarationResolver builds a Registry object.
  */
class DeclarationResolver {
  // TODO: We could move all this state into the resolve function and make it implicit.

  private val typeDeclarations: mutable.HashMap[String, TypeDeclNode] = mutable.HashMap()
  private val multiFunctionDeclarations: mutable.HashMap[String, List[DeclNode.FunctionNode]] = mutable.HashMap()
  private val dependencyGraph: DependencyGraph = new DependencyGraph(new DependencyGraph.Owner {
    override def hasTypeDeclaration(name: String): Boolean = typeDeclarations.contains(name)
    override def getTypeDeclaration(name: String): Option[TypeDeclNode] = typeDeclarations.get(name)
  })

  /**
    * Register a type declaration and add its type to the dependency graph.
    */
  private def addTypeDeclaration(declaration: TypeDeclNode): Verification = {
    // Immediately stop the processing of this type declaration if the name is already taken.
    if (typeDeclarations.contains(declaration.name) || Type.predefinedTypes.contains(declaration.name)) {
      return Compilation.fail(TypeAlreadyExists(declaration))
    }

    val dependencyNames = declaration match {
      case TypeDeclNode.StructNode(_, implemented, _, members, _) =>
        implemented ++ members.filterType[TypeDeclNode.ComponentNode].map(_.name)
      case TypeDeclNode.TraitNode(_, extended, components, _) =>
        extended ++ components
    }
    dependencyNames.foreach(dependency => dependencyGraph.add(declaration.name, dependency))
    typeDeclarations.put(declaration.name, declaration)

    Verification.succeed
  }

  /**
    * Register a function declaration with the multi-function collection of the same name.
    */
  private def addFunctionDeclaration(declaration: DeclNode.FunctionNode): Verification = {
    multiFunctionDeclarations.updateWith(declaration.name) {
      case None => Some(List(declaration))
      case Some(functions) => Some(declaration :: functions)
    }
    Verification.succeed
  }

  /**
    * Builds the registry and body pool from all declarations.
    */
  def resolve(declarations: List[DeclNode]): Compilation[Registry] = {
    // Declare the registry as implicit now so that we don't have to break up the for-comprehension,
    // which sadly doesn't support implicit variable declarations.
    implicit lazy val registry: Registry = new Registry()
    implicit lazy val typeScope: TypeScope = registry.typeScope

    for {
      _ <- addDeclarations(declarations)

      // TODO: If we want to support external libraries, we should add these here, especially to the dependency graph.
      //       Supporting external libraries will need a redesign of some of the early aspects. I am assuming that the
      //       compiler will produce a manifest of all type and function signatures when it compiles a project. This
      //       manifest can then be used to add externally declared types and functions to the current project. However,
      //       we will have to redesign Definition structures so that constructor and function bodies are optional when
      //       a definition has been declared as "external".

      // We do three separate passes over type declarations:
      //  1. Resolve types.
      //  2. Resolve class/label definitions.
      //  3. Resolve constructors.
      // This has the distinct advantage that we don't need to defer typings of parameters and members, as all types
      // have been added to the registry by then, and we are not limited by the class definition not being available
      // when we generate additional constructors.

      typeResolutionOrder <- computeTypeResolutionOrder()
      _ <- resolveDeclaredTypesInOrder(typeResolutionOrder)
      _ <- resolveAliasTypes()
      _ <- resolveTypeDefinitionsInOrder(typeResolutionOrder)
      _ <- resolveMultiFunctions()
    } yield registry
  }

  private def addDeclarations(declarations: List[DeclNode]): Verification = {
    declarations.map {
      case function: DeclNode.FunctionNode => addFunctionDeclaration(function)
      case tpe: TypeDeclNode => addTypeDeclaration(tpe)
    }.simultaneous.verification
  }

  /**
    * Computes the correct order in which to resolve type declarations.
    */
  private def computeTypeResolutionOrder(): Compilation[List[TypeDeclNode]] = {
    dependencyGraph.computeTypeResolutionOrder().map { names =>
      if (names.head != "Any") {
        throw CompilationException("Any should be the first type in the type resolution order.")
      }

      // With .tail we exclude Any.
      names.tail.map(typeDeclarations(_))
    }
  }

  /**
    * Resolve all declared types in the proper resolution order and register them with the Registry.
    */
  private def resolveDeclaredTypesInOrder(typeResolutionOrder: List[TypeDeclNode])(implicit registry: Registry): Verification = {
    typeResolutionOrder.map { node =>
      (node match {
        case structNode: TypeDeclNode.StructNode => TypeResolver.resolve(structNode)
        case traitNode: TypeDeclNode.TraitNode => TypeResolver.resolve(traitNode)
      }).map(tpe => registry.registerType(tpe.name, tpe))
    }.simultaneous.verification
  }

  /**
    * Resolve all type definitions in the proper resolution order and register them with the Registry. This function
    * guarantees that definitions are returned in the type resolution order.
    */
  private def resolveTypeDefinitionsInOrder(typeResolutionOrder: List[TypeDeclNode])(implicit registry: Registry): Verification = {
    typeResolutionOrder.map { node =>
      (node match {
        case structNode: TypeDeclNode.StructNode => TraitDefinitionResolver.resolve(structNode)
        case traitNode: TypeDeclNode.TraitNode => StructDefinitionResolver.resolve(traitNode)
      }).map(registry.registerTypeDefinition)
    }.simultaneous.verification
  }

  /**
    * Resolves and registers all multi-functions.
    */
  private def resolveMultiFunctions()(implicit registry: Registry): Verification = {
    multiFunctionDeclarations.map { case (_, nodes) =>
      MultiFunctionDefinitionResolver.resolve(nodes).map(registry.registerMultiFunction)
    }.toList.simultaneous.verification
  }
}

object DeclarationResolver {
  case class TypeAlreadyExists(node: TypeDeclNode) extends Error(node) {
    override def message = s"The type ${node.name} is already declared somewhere else."
  }

  case class FunctionAlreadyExists(definition: FunctionDefinition) extends Error(definition) {
    override def message = s"The function ${definition.signature} is already declared somewhere else or has a type-theoretic duplicate."
  }
}
