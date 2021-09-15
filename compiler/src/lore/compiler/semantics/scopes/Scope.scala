package lore.compiler.semantics.scopes

import lore.compiler.core.Position
import lore.compiler.feedback.{Reporter, ScopeFeedback}
import lore.compiler.utils.CollectionExtensions.OptionExtension

import scala.collection.mutable

/**
  * A hierarchical scope resolving entries of some type A by name.
  */
trait Scope[A] {

  /**
    * The scope's parent, which is used as a fallback if an entry cannot be found in the current scope.
    */
  protected def optionalParent: Option[Scope[A]] = None

  /**
    * Fetches the entry with the given name from the current scope, disregarding any parent scopes.
    */
  protected def local(name: String): Option[A]

  /**
    * Fetches an entry with the given name from the closest scope.
    */
  def get(name: String): Option[A] = local(name).orElse(optionalParent.flatMap(_.get(name)))

  /**
    * Resolves an entry with the given name from the closest scope. If it cannot be found, an "unknown entry" error is
    * reported.
    */
  def resolve(name: String, position: Position)(implicit reporter: Reporter): Option[A] = {
    get(name).ifEmpty(reporter.error(ScopeFeedback.UnknownEntry(entryLabel, name, position)))
  }

  /**
    * The label of the entry this scope contains, such as a variable or a type. The label is used when "unknown entry"
    * and "already declared" (mutable scopes) errors are created. You may override this to generate better error messages.
    */
  protected def entryLabel: String = "entry"

}

trait ImmutableScope[A] extends Scope[A] {
  /**
    * All the entries of this scope.
    */
  protected def entries: Map[String, A]

  override protected def local(name: String): Option[A] = entries.get(name)
}

trait MutableScope[A] extends Scope[A] {
  protected val entries: mutable.Map[String, A] = new mutable.HashMap()

  override protected def local(name: String): Option[A] = entries.get(name)

  /**
    * Registers the given entry with the scope. If it is already registered in the current scope, an "already declared"
    * error is reported.
    */
  def register[B <: A](name: String, entry: B, position: Position)(implicit reporter: Reporter): Unit = {
    if (local(name).isDefined) {
      reporter.report(ScopeFeedback.AlreadyDeclared(entryLabel, name, position))
    } else {
      entries.put(name, entry)
    }
  }
}
