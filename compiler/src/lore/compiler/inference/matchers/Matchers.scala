package lore.compiler.inference.matchers

import lore.compiler.core.Compilation
import lore.compiler.core.Compilation.FoldCompilationsExtension
import lore.compiler.inference.Inference.Assignments
import lore.compiler.types.{TupleType, Type}

object Matchers {

  val unsupported: (Any, Any, Any, Any) => Nothing = (_, _, _, _) => { throw new UnsupportedOperationException }

  def matchTuple(
    t1: TupleType,
    t2: TupleType,
    assignments: Assignments,
    rec: (Assignments, Type, Type) => Compilation[Assignments],
    failure: Compilation[Nothing],
  ): Compilation[Assignments] = {
    if (t1.elements.size == t2.elements.size) {
      t1.elements.zip(t2.elements).foldCompiled(assignments) {
        case (assignments2, (e1, e2)) => rec(assignments2, e1, e2)
      }
    } else failure
  }

}
