package lore.compiler.phases.typing.inference

import lore.compiler.phases.typing.inference.Inference.Assignments
import lore.compiler.phases.typing.inference.InferenceBounds.BoundType
import lore.compiler.types.Type

/**
  * Inference variables are strictly reference-equal, much like type variables. They can be put in types in place of
  * other types and are resolved by the type inference algorithm.
  */
class InferenceVariable(val name: Option[String] = None) extends Type {
  override def equals(obj: Any): Boolean = obj match {
    case var2: InferenceVariable => this eq var2
    case _ => false
  }

  // TODO: This is only temporary!!
  lazy val actualName: String = name.getOrElse {
    InferenceVariable.nameCounter += 1
    s"iv${InferenceVariable.nameCounter}"
  }

  override def toString: String = actualName
}

object InferenceVariable {

  // TODO: This is only temporary!!
  protected var nameCounter = 0

  // TODO: Match the notions of "isDefined" with the different versions of instantiate. There should be an isDefined
  //       for lower, upper, lower AND upper, and candidate type versions.
  //       Maybe even call it `mayInstantiate`.

  /**
    * Whether the given inference variable is defined at all.
    */
  def isDefined(assignments: Assignments, inferenceVariable: InferenceVariable): Boolean = assignments.contains(inferenceVariable)

  /**
    * Whether the given inference variable is defined for the given bound.
    */
  def isDefinedAt(assignments: Assignments, inferenceVariable: InferenceVariable, boundType: BoundType): Boolean = {
    assignments.get(inferenceVariable).exists { bounds =>
      boundType match {
        case BoundType.Lower => bounds.lower.isDefined
        case BoundType.Upper => bounds.upper.isDefined
      }
    }
  }

  /**
    * Whether the given inference variable is defined for the given bounds.
    */
  def isDefinedAt(assignments: Assignments, inferenceVariable: InferenceVariable, boundTypes: Vector[BoundType]): Boolean = {
    boundTypes.forall(isDefinedAt(assignments, inferenceVariable, _))
  }

  /**
    * The bounds of the given inference variable, no matter if it's already defined or not.
    */
  def effectiveBounds(assignments: Assignments, inferenceVariable: InferenceVariable): InferenceBounds = {
    assignments.getOrElse(inferenceVariable, InferenceBounds(inferenceVariable, None, None))
  }

}
