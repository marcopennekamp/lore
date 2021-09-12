package lore.compiler.parsing

import lore.compiler.core.Fragment
import lore.compiler.feedback.Reporter
import lore.compiler.syntax.DeclNode
import lore.compiler.utils.ExecutionContexts.default

import scala.concurrent.duration.Duration
import scala.concurrent.{Await, Future}

object ParsingPhase {

  /**
    * Parses all fragments, resulting in a list of fragment module nodes. Each fragment is parsed as a module.
    */
  def process(fragments: Vector[Fragment])(implicit reporter: Reporter): Vector[DeclNode.ModuleNode] = {
    val future = Future.traverse(fragments)(processAsync)
    Await.result(future, Duration.Inf)
  }

  def process(fragment: Fragment)(implicit reporter: Reporter): DeclNode.ModuleNode = new FragmentParser()(fragment).parse()

  private def processAsync(fragment: Fragment)(implicit reporter: Reporter): Future[DeclNode.ModuleNode] = Future {
    process(fragment)
  }

}
