package lore.compiler.typing

import lore.compiler.core.{CompilationException, Position}
import lore.compiler.feedback.{Feedback, MemoReporter, Reporter, TypingFeedback}
import lore.compiler.semantics.definitions.BindingDefinition
import lore.compiler.semantics.modules.MultiReference

object MultiReferenceTyping {

  // TODO: `disambiguate` eats a lot of reported errors. We should improve this situation, especially in cases where
  //       there's one local and one global binding to consider. We could also provide more information with the
  //       `AmbiguousMultiReference` error regardless of the number of errors. `AmbiguousMultiReference` should both
  //       report successful and unsuccessful candidates.

  /**
    * Tries to disambiguate the multi-reference. All bindings of the multi-function are processed with `process`, in
    * two layers: local and global. If a layer produces exactly one result, the layer succeeds and the result is
    * returned. If the local layer fails, the global layer is attempted. If that fails as well, an appropriate error is
    * reported, depending on whether none or multiple results were produced.
    */
  def disambiguate[A <: BindingDefinition, R](
    multiReference: MultiReference[A],
    position: Position,
  )(
    process: (A, Reporter) => Option[R],
  )(implicit parentReporter: Reporter): Option[R] = {
    if (multiReference.isSingle) {
      throw CompilationException(s"A single-binding multi-reference shouldn't need to be disambiguated. Position:" +
        s" $position.")
    }

    def attempt(binding: A) = {
      val reporter = MemoReporter()
      process(binding, reporter).map {
        result => (result, binding, reporter.feedback)
      }
    }

    def finalize(result: R, feedback: Vector[Feedback]) = {
      parentReporter.report(feedback)
      Some(result)
    }

    val localResults = multiReference.local.flatMap(attempt)
    localResults match {
      case Vector((result, _, feedback)) => finalize(result, feedback)
      case _ =>
        if (multiReference.global.isEmpty) {
          // Report an ambiguity right away if there are no global bindings to consider.
          parentReporter.error(
            TypingFeedback.AmbiguousMultiReference(
              multiReference,
              localResults.map(_._2),
              Vector.empty,
              position,
            )
          )
          None
        } else {
          val globalResults = multiReference.global.flatMap(attempt)
          globalResults match {
            case Vector((result, _, feedback)) => finalize(result, feedback)
            case _ =>
              // Report an ambiguity taking local and global bindings into account.
              parentReporter.error(
                TypingFeedback.AmbiguousMultiReference(
                  multiReference,
                  localResults.map(_._2),
                  globalResults.map(_._2),
                  position,
                )
              )
              None
          }
        }
    }
  }

}
