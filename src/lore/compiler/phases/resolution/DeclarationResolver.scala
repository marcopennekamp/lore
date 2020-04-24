package lore.compiler.phases.resolution

import lore.ast.{DeclNode, TypeDeclNode}
import lore.compiler.Compilation.{C, Verification}
import lore.compiler.feedback.Error
import lore.compiler.phases.resolution.DeclarationResolver.{InheritanceCycle, TypeAlreadyExists}
import lore.compiler.{Compilation, Fragment, Registry, TypeExpressionEvaluator}
import lore.definitions.{ClassDefinition, MultiFunctionDefinition}
import lore.types.Type
import scalax.collection.GraphEdge._
import scalax.collection.mutable.Graph

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
import scala.collection.mutable

class DeclarationResolver {
  private val typeDeclarations: mutable.HashMap[String, FragmentNode[TypeDeclNode]] = mutable.HashMap()
  private var aliasDeclarations: List[FragmentNode[TypeDeclNode.AliasNode]] = List.empty
  private val multiFunctionDeclarations: mutable.HashMap[String, List[FragmentNode[DeclNode.FunctionNode]]] = mutable.HashMap()

  /**
    * A mutable dependency graph, the nodes being type names. A type Any is the root of all declared types. Edges are
    * directed from supertype to subtype.
    */
  private val dependencyGraph: Graph[String, DiEdge] = Graph()
  private implicit val edgelordFactory = DiEdge

  private def addTypeDeclaration(declaration: FragmentNode[TypeDeclNode]): Verification = {
    // Immediately stop the processing of this type declaration if the name is already taken.
    if (typeDeclarations.contains(declaration.node.name) || Type.predefinedTypes.contains(declaration.node.name)) {
      return Compilation.fail(TypeAlreadyExists(declaration.node)(declaration.fragment))
    }

    declaration.node match {
      case aliasNode: TypeDeclNode.AliasNode =>
        typeDeclarations.put(aliasNode.name, declaration)
        aliasDeclarations = declaration.asInstanceOf[FragmentNode[TypeDeclNode.AliasNode]] :: aliasDeclarations
      case declaredNode: TypeDeclNode.DeclaredNode => // Covers labels and classes.
        typeDeclarations.put(declaredNode.name, declaration)
        dependencyGraph.addEdge(declaredNode.supertypeName.getOrElse("Any"), declaredNode.name)
    }

    Verification.succeed
  }

  private def addFunctionDeclaration(declaration: FragmentNode[DeclNode.FunctionNode]): Verification = {
    multiFunctionDeclarations.updateWith(declaration.node.name) {
      case None => Some(List(declaration))
      case Some(functions) => Some(declaration :: functions)
    }
    Verification.succeed
  }

  /**
    * Adds a fragment to the declaration resolver.
    */
  private def addFragment(fragment: Fragment): Verification = {
    fragment.declarations.map {
      case function: DeclNode.FunctionNode => addFunctionDeclaration(FragmentNode(function, fragment))
      case tpe: TypeDeclNode => addTypeDeclaration(FragmentNode(tpe, fragment))
    }.simultaneous.map(_ => ())
  }

  /**
    * Find multiple cycles in the graph. Cycles with the same elements and order, but different starting nodes, are
    * only returned once. The function is useful for finding multiple dependency cycles so that users can quickly
    * fix them.
    *
    * This function is not guaranteed to find ALL cycles, but there is a good chance that it does.
    */
  private def distinctCycles: List[dependencyGraph.Cycle] = {
    var cycles = List.empty[dependencyGraph.Cycle]
    for (node <- dependencyGraph.nodes) {
      node.partOfCycle.foreach { cycle =>
        // We have found a cycle. Now we need to ensure that it isn't in the list yet.
        if (!cycles.exists(_.sameAs(cycle))) {
          cycles = cycle :: cycles
        }

        // This is not the most efficient way to compare cycles. We are operating on the assumption that dependency
        // cycles will be a rare occurrence: if a cycle happens, the programmer is inclined to fix it right away.
        // In the current implementation, any new cycle inserted into the list has to be compared to all other cycles.
        // That is fine if we don't find more than 100 cycles or so. Should we ever have performance issues stemming
        // from this section of the code, we can introduce a hashing function that is stable in respect to a cycle's
        // starting node, and thus likely throws only same cycles into the same bucket.
      }
    }
    cycles
  }

  /**
    * Builds the registry from all the declarations.
    */
  def buildRegistry(fragments: List[Fragment]): C[Registry] = {
    // First of all, we add all fragments to the data structures.
    val withAddedFragments = fragments.map(addFragment).simultaneous
    if (withAddedFragments.isError) return withAddedFragments.asInstanceOf[Compilation[Registry]]

    // TODO: If we want to support external libraries, we should add these here, especially to the dependency graph.
    //       Supporting external libraries will need a redesign of some of the early aspects. I am assuming that the
    //       compiler will produce a manifest of all type and function signatures when it compiles a project. This
    //       manifest can then be used to add externally declared types and functions to the current project. However,
    //       we will have to redesign Definition structures so that constructor and function bodies are optional when
    //       a definition has been declared as "external".

    // At this point, the dependency graph has been built. However, not all types in this graph will be valid, declared
    // types. As supertype declarations can refer to non-existent types, we have to take care that all names added to
    // the graph which don't have a corresponding type declaration must be removed again.
    for (node <- dependencyGraph.nodes) {
      if (node.value != "Any" && !typeDeclarations.contains(node.value)) {
        // The type was never declared and is thus invalid! We cannot, however, simply remove the node. The node will
        // have a dependant which should rather be Any for the purposes of this graph. For example, let's say we have
        // a type A that extends a type B. B doesn't exist. Then A should, for the purposes of cycle detection, derive
        // from Any.
        for (edge <- node.edges) {
          if (!(edge.from == node)) {
            throw new RuntimeException("An undeclared type name should not depend on any types itself in the dependency graph.")
          }
          val dependant = edge.to
          dependencyGraph.addEdge("Any", dependant)
        }
        dependencyGraph.remove(node.value)
      }
    }

    // We attempt to report as many cycles as possible so the user doesn't have to run the compiler multiple times
    // just to find all dependency cycles.
    if (dependencyGraph.isCyclic) {
      val cycles = distinctCycles
      assert(cycles.nonEmpty)
      return Compilation.fail(
        cycles.map { cycle =>
          val occurrence = typeDeclarations.getOrElse(
            cycle.startNode,
            throw new RuntimeException("Type declarations didn't contain a declaration that was part of the dependency graph."),
          )
          InheritanceCycle(cycle.nodes.map(_.value).toList, occurrence)
        }: _*
      )
    }

    // Now that spurious names have been removed and the graph has been shown not to have any cycles, since Any should
    // be the supertype of all declared types without a supertype, the graph should be connected. Note that we first
    // need to detect cycles, because if the graph has a dependency cycle, that component of the graph will not be
    // connected to Any, and thus the graph won't be connected.
    assert(dependencyGraph.isConnected)

    // At this point, we know our dependency graph is a directed, acyclic graph. We can start a topological sort.
    val typeResolutionOrder = dependencyGraph.topologicalSort.fold(
      _ => throw new RuntimeException(
        "Topological sort on the dependency graph found a cycle, even though we verified earlier that there was no such cycle."
      ),
      order => order.toList.map(_.value)
    )

    // The first element in the type resolution order must be Any, as it's the ultimate root type.
    assert(typeResolutionOrder.head == "Any")

    // Now that we have a proper order, we can start building the Registry.
    implicit val registry: Registry = new Registry()

    // First, we resolve all declared types in their proper resolution order.
    // With .tail, we exclude Any, since we don't need to add that to the registry, as it is already a part
    // of the predefined types.
    val withRegisteredDefinitions = typeResolutionOrder.tail.map { typeName =>
      assert(typeDeclarations.contains(typeName))
      implicit val FragmentNode(node, fragment) = typeDeclarations(typeName)
      node match {
        case _: TypeDeclNode.AliasNode =>
          throw new RuntimeException("At this point in the compilation step, an alias type should not be resolved.")
        case node: TypeDeclNode.DeclaredNode =>
          DeclaredTypeResolver.resolveDeclaredNode(node).map(registry.registerTypeDefinition)
      }
    }.simultaneous

    // Now that all declared types have been resolved, we can resolve alias types.
    // Note that this implicitly disallows cyclically defined alias types (which is the correct behavior). For example,
    // if we define an alias type like `type A = B | A`, there will already be an error: Type not found "A". Hence,
    // there is no reason to manually disallow self-references.
    val withResolvedAliasTypes = withRegisteredDefinitions.flatMap { _ =>
      aliasDeclarations.map { case FragmentNode(node, _fragment) =>
        implicit val fragment: Fragment = _fragment
        TypeExpressionEvaluator.evaluate(node.tpe).map(tpe => registry.registerType(node.name, tpe))
      }.simultaneous
    }

    // As you know, we deferred validating member and parameter types with TypingDeferred. We will have to do this now,
    // to ensure that all types can be resolved correctly. Simultaneously, we also resolve and register functions.
    // We do this in parallel to report all "type could not be found" errors that can be found at this stage.
    val withVerifiedTypingsAndRegisteredFunctions = withResolvedAliasTypes.flatMap { _ =>
      (
        // Resolve deferred typings.
        registry.getTypeDefinitions.values.map {
          case definition: ClassDefinition => DeferredTypingResolver.resolveDeferredTypings(definition)
          case _ => Verification.succeed // We do not have to verify any deferred typings for labels.
        }.toList.simultaneous,
        // Resolve and register functions.
        multiFunctionDeclarations.map { case (name, fragmentNodes) =>
          fragmentNodes.map { case FragmentNode(node, _fragment) =>
            implicit val fragment: Fragment = _fragment
            FunctionDeclarationResolver.resolveFunctionNode(node)
          }.simultaneous.map { functions =>
            val multiFunction = MultiFunctionDefinition(name, functions)
            registry.registerMultiFunction(multiFunction)
          }
        }.toList.simultaneous,
      ).simultaneous
    }

    withVerifiedTypingsAndRegisteredFunctions.map(_ => registry)
  }
}

object DeclarationResolver {
  case class TypeAlreadyExists(node: TypeDeclNode)(implicit fragment: Fragment) extends Error(node) {
    override def message = s"The type ${node.name} is already declared somewhere else."
  }

  /**
    * @param occurrence One of the type declarations where the cycles occurs, so that we can report one error location.
    */
  case class InheritanceCycle(cycle: List[String], occurrence: FragmentNode[TypeDeclNode]) extends Error(occurrence) {
    override def message: String = s"""
                                      |An inheritance cycle between the following types has been detected: ${cycle.mkString(", ")}.
                                      |A class or label A cannot inherit from a class/label B that also inherits from A directly or indirectly. The
                                      |subtyping relationships of declared types must result in a directed, acyclic graph.
    """.stripMargin.replaceAll("\n", " ").trim
  }
}
