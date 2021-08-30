package lore.compiler.parsing

import lore.compiler.core.Fragment
import lore.compiler.feedback.Reporter
import lore.compiler.syntax.DeclNode
import lore.compiler.utils.ExecutionContexts.default

import scala.concurrent.duration.Duration
import scala.concurrent.{Await, Future}

object ParsingPhase {

  /**
    * Parses all fragments, resulting in a flattened list of declaration nodes.
    */
  def process(fragments: Vector[Fragment])(implicit reporter: Reporter): Vector[DeclNode] = {
    val future = Future.foldLeft(fragments.map(processAsync))(Vector.empty[DeclNode])(_ ++ _)
    Await.result(future, Duration.Inf)
  }

  def process(fragment: Fragment)(implicit reporter: Reporter): Vector[DeclNode] = new FragmentParser()(fragment).parse()

  private def processAsync(fragment: Fragment)(implicit reporter: Reporter): Future[Vector[DeclNode]] = Future {
    process(fragment)
  }

}
