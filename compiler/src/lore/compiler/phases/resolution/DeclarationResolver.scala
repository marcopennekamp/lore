package lore.compiler.phases.resolution

import lore.compiler.core.Compilation.Verification
import lore.compiler.core.{Compilation, CompilationException, Error}
import lore.compiler.phases.resolution.DeclarationResolver.TypeAlreadyExists
import lore.compiler.semantics.functions.FunctionDefinition
import lore.compiler.semantics.{Registry, TypeScope}
import lore.compiler.syntax.{DeclNode, TypeDeclNode, TypeExprNode}
import lore.compiler.types.Type

import scala.collection.mutable

/**
  * Declarations can occur unordered. This is not only true within a file, but especially across multiple files. We
  * can declare trait hierarchies across Lore files, define functions on their own, and so on. The way in which
  * types and definitions are represented within the compiler requires us to find an order in which we can properly
  * resolve these types, especially traits, structs, and type aliases.
  *
  * The DeclarationResolver holds all top-level declarations across all files parsed by the Lore compiler
  * still in AST form. While files are being parsed, it builds a hierarchy of types, which gives us the correct order
  * in which we should, for example, build struct types and definitions. Once all type declarations have been
  * turned into definitions, the DeclarationResolver builds a Registry object.
  */
class DeclarationResolver {
  // TODO: We could move all this state into the resolve function and make it implicit.

  private val typeDeclarations: mutable.HashMap[String, TypeDeclNode] = mutable.HashMap()
  private val multiFunctionDeclarations: mutable.HashMap[String, Vector[DeclNode.FunctionNode]] = mutable.HashMap()
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

    // We have to filter out predefined types, as they may be mentioned in alias types or extended shape types,
    // but do not need to take part in any declaration order.
    // TODO (shape): Once we add extending shape types, we might have to account for them in the dependency hierarchy.
    val dependencyNames = (declaration match {
      case TypeDeclNode.AliasNode(_, expr, _) => TypeExprNode.identifiers(expr)
      case TypeDeclNode.StructNode(_, implemented, _, _) => implemented
      case TypeDeclNode.TraitNode(_, extended, _) => extended
    }).filterNot(Type.predefinedTypes.contains)

    if (dependencyNames.nonEmpty) {
      dependencyNames.foreach(dependency => dependencyGraph.add(declaration.name, dependency))
    } else {
      dependencyGraph.add(declaration.name, "Any")
    }

    typeDeclarations.put(declaration.name, declaration)
    Verification.succeed
  }

  /**
    * Register a function declaration with the multi-function collection of the same name.
    */
  private def addFunctionDeclaration(declaration: DeclNode.FunctionNode): Verification = {
    multiFunctionDeclarations.updateWith(declaration.name) {
      case None => Some(Vector(declaration))
      case Some(functions) => Some(functions :+ declaration)
    }
    Verification.succeed
  }

  /**
    * Builds the registry from all declarations.
    */
  def resolve(declarations: Vector[DeclNode]): Compilation[Registry] = {
    // Declare the registry as implicit now so that we don't have to break up the for-comprehension,
    // which sadly doesn't support implicit variable declarations.
    implicit lazy val registry: Registry = new Registry()
    implicit lazy val typeScope: TypeScope = registry.typeScope

    for {
      _ <- addDeclarations(declarations)

      // We perform two separate passes over type declarations:
      //  1. Resolve types.
      //  2. Resolve struct/trait definitions.
      // This has the distinct advantage that we don't need to defer typings of parameters and members, as all types
      // have been added to the registry by then.

      typeResolutionOrder <- computeTypeResolutionOrder()
      _ <- resolveTypeDeclarationsInOrder(typeResolutionOrder)
      _ <- resolveTypeDefinitionsInOrder(typeResolutionOrder)
      _ <- resolveMultiFunctions()
    } yield registry
  }

  private def addDeclarations(declarations: Vector[DeclNode]): Verification = {
    declarations.map {
      case function: DeclNode.FunctionNode => addFunctionDeclaration(function)
      case tpe: TypeDeclNode => addTypeDeclaration(tpe)
    }.simultaneous.verification
  }

  /**
    * Computes the correct order in which to resolve type declarations.
    */
  private def computeTypeResolutionOrder(): Compilation[Vector[TypeDeclNode]] = {
    dependencyGraph.computeTypeResolutionOrder().map { names =>
      if (names.head != "Any") {
        throw CompilationException("Any should be the first type in the type resolution order.")
      }

      // With .tail we exclude Any.
      names.tail.map(typeDeclarations(_))
    }
  }

  /**
    * Resolve all type declarations in the proper resolution order and register them with the Registry.
    */
  private def resolveTypeDeclarationsInOrder(typeResolutionOrder: Vector[TypeDeclNode])(implicit registry: Registry): Verification = {
    typeResolutionOrder.map { node =>
      (node match {
        case aliasNode: TypeDeclNode.AliasNode => TypeResolver.resolve(aliasNode).map(tpe => (aliasNode.name, tpe))
        case structNode: TypeDeclNode.StructNode => TypeResolver.resolve(structNode).map(tpe => (tpe.name, tpe))
        case traitNode: TypeDeclNode.TraitNode => TypeResolver.resolve(traitNode).map(tpe => (tpe.name, tpe))
      }).map { case (name, tpe) => registry.registerType(name, tpe) }
    }.simultaneous.verification
  }

  /**
    * Resolve all type definitions in the proper resolution order and register them with the Registry. This function
    * guarantees that definitions are returned in the type resolution order.
    */
  private def resolveTypeDefinitionsInOrder(typeResolutionOrder: Vector[TypeDeclNode])(implicit registry: Registry): Verification = {
    typeResolutionOrder.map { node =>
      (node match {
        case _: TypeDeclNode.AliasNode => None
        case structNode: TypeDeclNode.StructNode => Some(StructDefinitionResolver.resolve(structNode))
        case traitNode: TypeDeclNode.TraitNode => Some(TraitDefinitionResolver.resolve(traitNode))
      }).map(_.map(registry.registerTypeDefinition)).toCompiledOption
    }.simultaneous.verification
  }

  /**
    * Resolves and registers all multi-functions.
    */
  private def resolveMultiFunctions()(implicit registry: Registry): Verification = {
    multiFunctionDeclarations.map { case (_, nodes) =>
      MultiFunctionDefinitionResolver.resolve(nodes).map(registry.registerMultiFunction)
    }.toVector.simultaneous.verification
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
