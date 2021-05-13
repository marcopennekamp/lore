package lore.compiler.phases.transformation.inference.resolvers
import lore.compiler.core.Compilation
import lore.compiler.phases.transformation.inference.Inference.Assignments
import lore.compiler.phases.transformation.inference.{TypingJudgment, Unification}
import lore.compiler.semantics.Registry

object EqualsJudgmentResolver extends JudgmentResolver.Nondirectional[TypingJudgment.Equals] {

  override def nondirectional(
    judgment: TypingJudgment.Equals,
    assignments: Assignments,
  )(implicit registry: Registry): Compilation[Assignments] = {
    // The direction is unimportant as we rely on unification.
    Unification.unify(assignments, judgment.t1, judgment.t2, judgment)
  }

}
