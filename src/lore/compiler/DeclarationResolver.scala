package lore.compiler

import lore.ast.{DeclNode, TypeDeclNode}
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
  private var aliasDeclarations: List[TypeDeclNode.AliasNode] = List.empty
  private val multiFunctionDeclarations: mutable.HashMap[String, List[FragmentNode[DeclNode.FunctionNode]]] = mutable.HashMap()

  /**
    * A mutable dependency graph, the nodes being type names. A type Any is the root of all declared types. Edges are
    * directed from supertype to subtype.
    */
  private val dependencyGraph: Graph[String, DiEdge] = Graph()
  private implicit val edgelordFactory = DiEdge

  def addTypeDeclaration(declaration: FragmentNode[TypeDeclNode]): C[Unit] = {
    // Immediately stop the processing of this type declaration if the name is already taken.
    if (typeDeclarations.contains(declaration.node.name) || Type.predefinedTypes.contains(declaration.node.name)) {
      return Compilation.fail(Error.TypeAlreadyExists(declaration.node))
    }

    declaration.node match {
      case aliasNode: TypeDeclNode.AliasNode =>
        typeDeclarations.put(aliasNode.name, declaration)
        aliasDeclarations = aliasNode :: aliasDeclarations
        // TODO: Disallow cycles for alias types. (For now.)
        //       Actually, if we define an alias type like `type A = B | A`, there will already be an error:
        //       Type not found "A". So we might not actually need to change anything.
      case declaredNode: TypeDeclNode.DeclaredNode => // Covers labels and classes.
        typeDeclarations.put(declaredNode.name, declaration)
        dependencyGraph.addEdge(declaredNode.supertypeName.getOrElse("Any"), declaredNode.name)
    }

    Compilation.succeed(())
  }

  def addFunctionDeclaration(declaration: FragmentNode[DeclNode.FunctionNode]): C[Unit] = {
    multiFunctionDeclarations.updateWith(declaration.node.name) {
      case None => Some(List(declaration))
      case Some(functions) => Some(declaration :: functions)
    }
    Compilation.succeed(())
  }

  /**
    * Adds a fragment to the declaration resolver. Also associates feedback received up to here with the given fragment.
    */
  def addFragment(fragment: Fragment): C[Unit] = {
    fragment.declarations.map {
      case function: DeclNode.FunctionNode => addFunctionDeclaration(FragmentNode(function, fragment))
      case tpe: TypeDeclNode => addTypeDeclaration(FragmentNode(tpe, fragment))
    }.combine.map(_ => ()).applyToFeedback(_.associate(fragment))
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
  def buildRegistry(): C[Registry] = {
    // At this point, the dependency graph has been built. Since Any is the supertype of all declared types without
    // a supertype, the graph should be connected.
    assert(dependencyGraph.isConnected)

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
          // These errors are being associated with their fragments through the passed FragmentNode occurrence.
          Error.InheritanceCycle(cycle.nodes.map(_.value).toList, occurrence)
        }: _*
      )
    }

    // At this point, we know our dependency graph is a directed, acyclic graph. We can start a topological sort.
    val typeResolutionOrder = dependencyGraph.topologicalSort.fold(
      _ => throw new RuntimeException(
        "Topological sort on the dependency graph found a cycle, even though we verified earlier that there was no such cycle."
      ),
      order => order.toList.map(_.value)
    )

    // The first element in the type resolution order must be Any, as it's the ultimate root type.
    assert(typeResolutionOrder.head == "Any")

    // TODO: Idea: Build a graph with type declarations as nodes and edges being subtyping relationships. Then perform
    //       topographic sort.
    ???
  }
}
