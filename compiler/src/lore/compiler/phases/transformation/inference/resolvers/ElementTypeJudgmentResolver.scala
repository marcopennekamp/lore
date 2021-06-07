package lore.compiler.phases.transformation.inference.resolvers

import lore.compiler.core.Compilation
import lore.compiler.phases.transformation.inference.Inference.{Assignments, instantiate}
import lore.compiler.phases.transformation.inference.InferenceBounds.narrowBounds
import lore.compiler.phases.transformation.inference.TypingJudgment
import lore.compiler.phases.transformation.inference.resolvers.JudgmentResolver.CollectionExpected
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
      case _ => Compilation.fail(CollectionExpected(instantiatedCollection, judgment.position))
    }
    elementType.flatMap(tpe => narrowBounds(assignments, judgment.target, tpe, tpe, judgment))
  }

}
