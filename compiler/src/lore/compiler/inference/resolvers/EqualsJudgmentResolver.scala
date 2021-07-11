package lore.compiler.inference.resolvers
import lore.compiler.feedback.Reporter
import lore.compiler.inference.Inference.Assignments
import lore.compiler.inference.{TypingJudgment, Unification}
import lore.compiler.semantics.Registry

object EqualsJudgmentResolver extends JudgmentResolver.Nondirectional[TypingJudgment.Equals] {

  override def nondirectional(
    judgment: TypingJudgment.Equals,
    assignments: Assignments,
  )(implicit registry: Registry, reporter: Reporter): Option[Assignments] = {
    // The direction is unimportant as we rely on unification.
    Unification.unify(judgment.t1, judgment.t2, assignments, judgment)
  }

}
