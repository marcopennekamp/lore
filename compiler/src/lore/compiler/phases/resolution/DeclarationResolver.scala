package lore.compiler.phases.resolution

import lore.compiler.syntax.{DeclNode, TypeDeclNode}
import lore.compiler.core.Compilation.{C, Verification}
import lore.compiler.core.{Compilation, CompilationException, Error}
import lore.compiler.semantics.functions.{FunctionDefinition, MultiFunctionDefinition}
import lore.compiler.phases.resolution.DeclarationResolver.{FunctionAlreadyExists, TypeAlreadyExists}
import lore.compiler.semantics.{Registry, TypeScope}
import lore.compiler.semantics.structures.ClassDefinition
import lore.compiler.types.Type

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
  private val typeDeclarations: mutable.HashMap[String, TypeDeclNode] = mutable.HashMap()
  private var aliasDeclarations: List[TypeDeclNode.AliasNode] = List.empty
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

    declaration match {
      case aliasNode: TypeDeclNode.AliasNode =>
        typeDeclarations.put(aliasNode.name, declaration)
        aliasDeclarations = aliasNode :: aliasDeclarations
      case declaredNode: TypeDeclNode.DeclaredNode => // Covers labels and classes.
        typeDeclarations.put(declaredNode.name, declaration)
        dependencyGraph.add(declaredNode.name, declaredNode.supertypeName.getOrElse("Any"))
    }

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
    * Builds the registry from all the declarations.
    */
  def buildRegistry(declarations: List[DeclNode]): C[Registry] = {
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

      typeResolutionOrder <- dependencyGraph.computeTypeResolutionOrder()
      _ <- resolveDeclaredTypesInOrder(typeResolutionOrder)
      _ <- resolveAliasTypes()
      _ <- (resolveDeferredTypings(), resolveFunctions()).simultaneous
    } yield registry
  }

  private def addDeclarations(declarations: List[DeclNode]): Verification = {
    declarations.map {
      case function: DeclNode.FunctionNode => addFunctionDeclaration(function)
      case tpe: TypeDeclNode => addTypeDeclaration(tpe)
    }.simultaneous.verification
  }

  /**
    * Resolve all declared types in the proper resolution order and register them with the Registry.
    */
  private def resolveDeclaredTypesInOrder(typeResolutionOrder: List[String])(implicit registry: Registry): Verification = {
    // With .tail, we exclude Any, since we don't need to add that to the registry, as it is already a part
    // of the predefined types.
    typeResolutionOrder.tail.map { typeName =>
      assert(typeDeclarations.contains(typeName))
      typeDeclarations(typeName) match {
        case _: TypeDeclNode.AliasNode =>
          throw CompilationException("At this point in the compilation step, an alias type should not be resolved.")
        case node: TypeDeclNode.DeclaredNode =>
          DeclaredTypeResolver.resolveDeclaredNode(node).map(registry.registerTypeDefinition)
      }
    }.simultaneous.verification
  }

  /**
    * Resolve all alias types and register them in the Registry.
    *
    * Note that this implicitly disallows cyclically defined alias types (which is the correct behavior). For example,
    * if we define an alias type like `type A = B | A`, there will already be an error: Type not found "A". Hence,
    * there is no reason to manually disallow self-references.
    */
  private def resolveAliasTypes()(implicit registry: Registry): Verification = {
    aliasDeclarations.map { node =>
      //TypeExpressionEvaluator.evaluate(node.tpe).map(tpe => registry.registerType(node.name, tpe))
      // TODO: Implement alias types as named types.
      Compilation.fail(new Error(node) {
        override def message: String = "Alias types are currently not supported."
      })
    }.simultaneous.verification
  }

  /**
    * Resolves all typings that have been deferred until after all types are declared.
    */
  private def resolveDeferredTypings()(implicit registry: Registry): Verification = {
    registry.getTypeDefinitions.values.map {
      case definition: ClassDefinition => DeferredTypingResolver.resolveDeferredTypings(definition)
      case _ => Verification.succeed // We do not have to verify any deferred typings for labels.
    }.toList.simultaneous.verification
  }

  /**
    * Resolves and registers all functions. Also returns a compilation error if a function signature is not
    * unique.
    */
  private def resolveFunctions()(implicit registry: Registry): Verification = {
    multiFunctionDeclarations.map { case (name, nodes) =>
      for {
        functions <- nodes.map { node =>
          FunctionDeclarationResolver.resolveFunctionNode(node)
        }.simultaneous
        _ <- verifyFunctionsUnique(functions)
      } yield {
        val multiFunction = MultiFunctionDefinition(name, functions)
        registry.registerMultiFunction(multiFunction)
      }
    }.toList.simultaneous.verification
  }

  /**
    * Verifies that all functions declared in the multi-function have a unique signature, which means that their
    * input types aren't equally specific. If they are, multiple dispatch won't be able to differentiate between
    * such two functions, and hence they can't be valid.
    */
  private def verifyFunctionsUnique(functions: List[FunctionDefinition]): Verification = {
    // Of course, all functions added to the multi-function must have the same name. If that is not the case,
    // there is something very wrong with the compiler.
    if (functions.size > 1) {
      functions.sliding(2).forall { case List(a, b) => a.name == b.name }
    }

    // Then verify that all functions have different signatures.
    functions.map { function =>
      // We decide "duplicity" based on the specificity two functions would have in a multi-function fit context.
      // That is, if two functions are equally specific, they are effectively the same in the eyes of multiple
      // dispatch. This is what we want to avoid by verifying that all functions are "unique".
      val containsDuplicate = functions.filterNot(_ == function).exists(_.signature.isEquallySpecific(function.signature))
      if (containsDuplicate) {
        Compilation.fail(FunctionAlreadyExists(function))
      } else {
        Verification.succeed
      }
    }.simultaneous.verification
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
