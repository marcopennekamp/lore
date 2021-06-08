package lore.compiler.inference.matchers

import lore.compiler.core.Compilation
import lore.compiler.inference.Inference.Assignments
import lore.compiler.types.{ProductType, Type}

object Matchers {

  val unsupported: (Any, Any, Any, Any) => Nothing = (_, _, _, _) => { throw new UnsupportedOperationException }

  def matchTuple(
    t1: ProductType,
    t2: ProductType,
    assignments: Assignments,
    rec: (Assignments, Type, Type) => Compilation[Assignments],
    failure: Compilation[Nothing],
  ): Compilation[Assignments] = {
    if (t1.elements.size == t2.elements.size) {
      t1.elements.zip(t2.elements).foldLeft(Compilation.succeed(assignments)) {
        case (compilation, (e1, e2)) => compilation.flatMap(rec(_, e1, e2))
      }
    } else failure
  }

}
