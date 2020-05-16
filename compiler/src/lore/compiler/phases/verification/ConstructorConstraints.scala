package lore.compiler.phases.verification

import lore.compiler.ast.StmtNode.ReturnNode
import lore.compiler.ast.{ExprNode, StmtNode, TopLevelExprNode}
import lore.compiler.ast.visitor.{StmtVisitor, VerificationStmtVisitor}
import lore.compiler.{Compilation, Fragment}
import lore.compiler.Compilation.Verification
import lore.compiler.feedback.Error
import lore.compiler.definitions.{ClassDefinition, ConstructorDefinition}
import scalax.collection.GraphEdge.DiEdge
import scalax.collection.mutable.Graph

object ConstructorConstraints {

  /**
    * Verifies all constructor constraints for the given class.
    */
  def verify(definition: ClassDefinition): Verification = {
    (
      verifyContinuations(definition),
      definition.constructors.map(verifyNoReturn).simultaneous,
    ).simultaneous.verification
  }

  case class ConstructorMustEndInContinuation(definition: ClassDefinition, constructor: ConstructorDefinition) extends Error(constructor) {
    override def message = s"The constructor ${constructor.name} of the class ${definition.name} should end in a continuation."
  }

  case class ContinuationCallsAreCyclic(definition: ClassDefinition) extends Error(definition) {
    override def message = s"Constructor calls within the class ${definition.name} are cyclic."
  }

  case class ContinuationsMustEndInConstruct(definition: ClassDefinition, constructor: ConstructorDefinition) extends Error(constructor) {
    override def message = s"The ${definition.name} construction chain starting with the constructor ${constructor.name} must end in a construct call, but doesn't."
  }

  /**
    * Verifies that the constructors end in a continuation, that no continuation appears in any other places, and
    * that continuations are acyclic and end in a construct continuation.
    */
  def verifyContinuations(definition: ClassDefinition): Verification = {
    implicit val fragment: Fragment = definition.position.fragment

    // We check first that all constructors end in a continuation and that no continuation appears in any other places.
    // This is deliberately followed by a flatMap, because we don't want to check the graph parts of this verification
    // if not all continuations are in the right spot.
    val correctPlacement = definition.constructors.map { constructor =>
      val statements = constructor.bodyBlock.statements
      val endsInContinuation = if (!statements.lastOption.exists(_.isInstanceOf[TopLevelExprNode.ContinuationNode])) {
        Compilation.fail(ConstructorMustEndInContinuation(definition, constructor))
      } else Verification.succeed
      // Visit all the other nodes except the last one (which should be a continuation) and check that they ARE NOT
      // a continuation.
      val noContinuationOtherwise = StmtVisitor.visit(new NoContinuationVisitor()) {
        ExprNode.BlockNode(statements.dropRight(1))
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
        val continuation = constructor.bodyBlock.statements.last.asInstanceOf[TopLevelExprNode.ContinuationNode]
        continuation match {
          case TopLevelExprNode.ConstructorCallNode(name, _) =>
            flowGraph.addEdge(constructor.name, name.getOrElse(definition.name))
          case TopLevelExprNode.ConstructNode(_, _) =>
            flowGraph.addEdge(constructor.name, constructName)
        }
      }

      // Now we first verify that the flow graph is acyclic.
      val isCyclic = if (flowGraph.isCyclic) {
        Compilation.fail(ContinuationCallsAreCyclic(definition))
      } else Verification.succeed

      isCyclic.flatMap { _ =>
        // TODO: Do we even need to verify this? Or do the properties that all constructors have to end in a
        //       continuation and that no cycles may exist suffice to also prove this property?
        // And then we can verify that every call ends in a construct continuation.
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

  case class NoReturnInConstructor(node: ReturnNode)(implicit fragment: Fragment) extends Error(node) {
    override def message = s"A constructor may not contain a return statement."
  }

  /**
    * Verifies that the constructor contains no return statement.
    */
  def verifyNoReturn(constructor: ConstructorDefinition): Verification = {
    implicit val fragment: Fragment = constructor.position.fragment
    val visitor = new VerificationStmtVisitor {
      override def verify(node: StmtNode): Verification = node match {
        case node@StmtNode.ReturnNode(_) => Compilation.fail(NoReturnInConstructor(node))
        case _ => Verification.succeed
      }
    }
    StmtVisitor.visit(visitor)(constructor.bodyBlock)
  }

}
