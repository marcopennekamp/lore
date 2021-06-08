package lore.compiler.inference.resolvers

import lore.compiler.core.Compilation
import lore.compiler.feedback.TypingFeedback.CollectionExpected
import lore.compiler.inference.Inference.{Assignments, instantiate}
import lore.compiler.inference.InferenceBounds.narrowBounds
import lore.compiler.inference.TypingJudgment
import lore.compiler.semantics.Registry
import lore.compiler.types.{ListType, MapType, ProductType}

object ElementTypeJudgmentResolver extends JudgmentResolver[TypingJudgment.ElementType] {

  override def forwards(
    judgment: TypingJudgment.ElementType,
    assignments: Assignments,
  )(implicit registry: Registry): Compilation[Assignments] = {
    val instantiatedCollection = instantiate(assignments, judgment.collection, _.candidateType)
    val elementType = instantiatedCollection match {
      case ListType(element) => Compilation.succeed(element)
      case MapType(key, value) => Compilation.succeed(ProductType(Vector(key, value)))
      case _ => Compilation.fail(CollectionExpected(instantiatedCollection, judgment))
    }
    elementType.flatMap(tpe => narrowBounds(assignments, judgment.target, tpe, judgment))
  }

}
