package lore.compiler.phases.transformation.inference

import lore.compiler.phases.transformation.inference.Inference.Assignments
import lore.compiler.types.{BasicType, Type}

import java.util.concurrent.atomic.AtomicInteger

/**
  * Inference variables are strictly reference-equal, much like type variables. They can be put in types in place of
  * other types and are resolved by the type inference algorithm.
  */
class InferenceVariable(val name: Option[String] = None) extends Type {
  override def equals(obj: Any): Boolean = obj match {
    case var2: InferenceVariable => this eq var2
    case _ => false
  }

  private lazy val actualName: String = name.getOrElse {
    s"iv${InferenceVariable.nameCounter.incrementAndGet()}"
  }

  override def toString: String = actualName
}

object InferenceVariable {

  private val nameCounter: AtomicInteger = new AtomicInteger()

  /**
    * Whether the given inference variable is defined at all.
    */
  def isDefined(iv: InferenceVariable, assignments: Assignments): Boolean = assignments.contains(iv)

  /**
    * The bounds of the given inference variable, no matter if it's already defined or not.
    */
  def effectiveBounds(iv: InferenceVariable, assignments: Assignments): InferenceBounds = {
    assignments.getOrElse(iv, InferenceBounds(iv, BasicType.Nothing, BasicType.Any))
  }

}
