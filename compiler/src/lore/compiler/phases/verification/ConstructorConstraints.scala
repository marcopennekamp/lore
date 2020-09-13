package lore.compiler.phases.verification

import lore.compiler.core.Compilation.Verification
import lore.compiler.core.{Compilation, Error}
import lore.compiler.semantics.functions.ConstructorDefinition
import lore.compiler.semantics.structures.StructDefinition
import lore.compiler.syntax.StmtNode.ReturnNode
import lore.compiler.syntax.visitor.{StmtVisitor, VerificationStmtVisitor}
import lore.compiler.syntax.{ExprNode, StmtNode, TopLevelExprNode}
import scalax.collection.GraphEdge.DiEdge
import scalax.collection.mutable.Graph

object ConstructorConstraints {

  /**
    * Verifies all constructor constraints for the given class.
    */
  def verify(definition: StructDefinition): Verification = {
    (
      verifyContinuations(definition),
      definition.constructors.map(verifyNoReturn).simultaneous,
    ).simultaneous.verification
  }

  case class ContinuationRequired(definition: StructDefinition, constructor: ConstructorDefinition) extends Error(constructor) {
    override def message = s"The constructor ${constructor.name} of the class ${definition.name} should end in a continuation."
  }

  case class CyclicContinuations(definition: StructDefinition) extends Error(definition) {
    override def message = s"Constructor calls within the class ${definition.name} are cyclic."
  }

  case class ContinuationsMustEndInConstruct(definition: StructDefinition, constructor: ConstructorDefinition) extends Error(constructor) {
    override def message = s"The ${definition.name} construction chain starting with the constructor ${constructor.name} must end in a construct call, but doesn't."
  }

  /**
    * Verifies that the constructors end in a continuation, that no continuation appears in any other places, and
    * that continuations are acyclic and end in a construct continuation.
    */
  def verifyContinuations(definition: StructDefinition): Verification = {
    // We check first that all constructors end in a continuation and that no continuation appears in any other places.
    // This is deliberately followed by a flatMap, because we don't want to check the graph parts of this verification
    // if not all continuations are in the right spot.
    val correctPlacement = definition.constructors.map { constructor =>
      val statements = constructor.bodyNode.statements
      val endsInContinuation = if (!statements.lastOption.exists(_.isInstanceOf[TopLevelExprNode.ContinuationNode])) {
        Compilation.fail(ContinuationRequired(definition, constructor))
      } else Verification.succeed
      // Visit all the other nodes except the last one (which should be a continuation) and check that they ARE NOT
      // a continuation.
      val noContinuationOtherwise = StmtVisitor.visit(new NoContinuationVisitor()) {
        ExprNode.BlockNode(statements.dropRight(1), constructor.bodyNode.position)
      }
      (endsInContinuation, noContinuationOtherwise).simultaneous
    }.simultaneous

    // Now that placement has been verified, we can check continuation flow.
    val correctFlow = correctPlacement.flatMap { _ =>
      val flowGraph: Graph[String, DiEdge] = Graph()
      implicit val edgeFactory = DiEdge
      val constructName = "!construct"

      // We build a flow graph following constructor calls.
      definition.constructors.foreach { constructor =>
        // The cast is now safe because we have previously verified that the last expression in the block is
        // a continuation.
        val continuation = constructor.bodyNode.statements.last.asInstanceOf[TopLevelExprNode.ContinuationNode]
        continuation match {
          case TopLevelExprNode.ConstructorCallNode(name, _, _, _) =>
            flowGraph.addEdge(constructor.name, name.getOrElse(definition.name))
          case TopLevelExprNode.ConstructNode(_, _, _) =>
            flowGraph.addEdge(constructor.name, constructName)
        }
      }

      // Now we first verify that the flow graph is acyclic.
      val isCyclic = if (flowGraph.isCyclic) {
        Compilation.fail(CyclicContinuations(definition))
      } else Verification.succeed

      // And then we can verify that every call ends in a construct continuation. This may be covered by ensuring
      // that all constructors have to end in a continuation and that no cycles may exist, which we do, so this
      // check might never even throw an error, but a little redundancy never hurt anyone.
      isCyclic.flatMap { _ =>
        definition.constructors.map { constructor =>
          // We look for the constructor's node in the flow graph and then try to find a construct successor.
          flowGraph.get(constructor.name).findSuccessor(_.value == constructName) match {
            case Some(_) => Verification.succeed
            case None => Compilation.fail(ContinuationsMustEndInConstruct(definition, constructor))
          }
        }.simultaneous
      }
    }

    correctFlow.verification
  }

  case class NoReturnInConstructor(node: ReturnNode) extends Error(node) {
    override def message = s"A constructor may not contain a return statement."
  }

  /**
    * Verifies that the constructor contains no return statement.
    */
  def verifyNoReturn(constructor: ConstructorDefinition): Verification = {
    val visitor = new VerificationStmtVisitor {
      override def verify(node: StmtNode): Verification = node match {
        case node@StmtNode.ReturnNode(_, _) => Compilation.fail(NoReturnInConstructor(node))
        case _ => Verification.succeed
      }
    }
    StmtVisitor.visit(visitor)(constructor.bodyNode)
  }

}
