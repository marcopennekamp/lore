package lore.compiler.inference

import lore.compiler.core.{CompilationException, Position}
import lore.compiler.inference.Inference.Assignments
import lore.compiler.types.{BasicType, Type, TypeVariable}

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

  lazy val label: String = name.getOrElse {
    s"iv${InferenceVariable.nameCounter.incrementAndGet()}"
  }

  override def toString: String = label
}

object InferenceVariable {

  private val nameCounter: AtomicInteger = new AtomicInteger()

  /**
    * Get the bounds of the given inference variable. If the inference variable isn't contained in `assignments`, a
    * compilation exception is thrown.
    */
  def bounds(iv: InferenceVariable, assignments: Assignments): InferenceBounds = {
    assignments.getOrElse(iv, throw CompilationException(s"The bounds of inference variable $iv should have been defined by now."))
  }

  /**
    * Whether the given inference variable is fixed, which occurs when its bounds are fixed.
    */
  def isFixed(iv: InferenceVariable, assignments: Assignments): Boolean = InferenceBounds.areFixed(bounds(iv, assignments))

  /**
    * Represents the given type variables as inference variables in the returned assignments map, and builds the typing
    * judgments required to represent type variable bounds.
    */
  def fromTypeVariables(typeVariables: Vector[TypeVariable], position: Position): (Map[TypeVariable, InferenceVariable], Vector[TypingJudgment]) = {
    val assignments = typeVariables.map(tv => (tv, new InferenceVariable)).toMap
    val judgments = typeVariables.flatMap { tv =>
      var judgments = Vector.empty[TypingJudgment]

      if (tv.lowerBound != BasicType.Nothing) {
        judgments = judgments :+ TypingJudgment.Subtypes(Type.substitute(tv.lowerBound, assignments), assignments(tv), position)
      }

      if (tv.upperBound != BasicType.Any) {
        judgments = judgments :+ TypingJudgment.Subtypes(assignments(tv), Type.substitute(tv.upperBound, assignments), position)
      }

      judgments
    }
    (assignments, judgments)
  }

}
