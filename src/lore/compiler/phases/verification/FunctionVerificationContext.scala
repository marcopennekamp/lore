package lore.compiler.phases.verification

import lore.ast.ExprNode
import lore.compiler.Registry
import lore.compiler.phases.verification.FunctionVerificationContext.LoopContext
import lore.definitions.FunctionSignature
import lore.types.{ListType, ProductType, SumType, Type}

/**
  * A context for function verification passes, for example to hold local variable scopes.
  */
class FunctionVerificationContext(signature: FunctionSignature) {
  private var scopes: List[Scope] = List(new FunctionScope(signature))
  def currentScope: Scope = scopes.head
  def openScope(): Unit = {
    val scope = new BlockScope(currentScope)
    scopes = scope :: scopes
  }
  def closeScope(): Unit = {
    assert(scopes.length > 1) // The function scope should not be closed, hence > 1.
    scopes = scopes.tail
  }

  private var loopContexts: List[LoopContext] = Nil
  def currentLoopContext: Option[LoopContext] = loopContexts.headOption
  def pushLoopContext(): Unit = {
    loopContexts = new LoopContext :: loopContexts
  }
  def popLoopContext(): LoopContext = {
    assert(loopContexts.nonEmpty)
    val head = loopContexts.head
    loopContexts = loopContexts.tail
    head
  }
}

object FunctionVerificationContext {
  /**
    * A context that remembers all yielded expressions in the current loop's scope. This is used to calculate
    * the result type of the loop.
    */
  class LoopContext {
    private var yielded: List[ExprNode] = Nil
    def registerYielded(node: ExprNode): Unit = {
      yielded = node :: yielded
    }

    /**
      * Calculates the LIST TYPE of the loop result (so not just the element) by finding the least
      * upper bound type of all yielded expressions.
      */
    def listType(implicit registry: Registry): Type = {
      if (yielded.isEmpty) ProductType.UnitType // Per the spec, we return () if the list has no yields at all.
      else ListType(
        SumType.construct(yielded.map(_.inferredType)) match {
          case tpe@SumType(_) => tpe.join
          case t => t
        }
      )
    }
  }
}
