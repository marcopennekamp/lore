package lore.compiler.semantics.scopes

import lore.compiler.core.Compilation.{ToCompilationExtension, Verification}
import lore.compiler.core.{Compilation, CompilationException, Position}
import lore.compiler.feedback.Feedback
import lore.compiler.semantics.scopes.Scope.{AlreadyDeclared, UnknownEntry}

import scala.collection.mutable

/**
  * A hierarchical scope resolving entries of some type A by name.
  */
trait Scope[A] {

  /**
    * Fetches the entry with the given name from the current scope, disregarding any parent scopes.
    */
  protected def local(name: String): Option[A]

  /**
    * Adds the given entry to the scope.
    */
  protected def add(name: String, entry: A): Unit

  /**
    * The scope's parent, which is used as a fallback if an entry cannot be found in the current scope.
    */
  protected def parent: Option[Scope[A]] = None

  /**
    * Fetches an entry with the given name from the closest scope.
    */
  def get(name: String): Option[A] = local(name).orElse(parent.flatMap(_.get(name)))

  /**
    * Resolves an entry with the given name from the closest scope. If it cannot be found, we return a
    * compilation error.
    */
  def resolve(name: String)(implicit position: Position): Compilation[A] = {
    get(name) match {
      case None => Compilation.fail(unknownEntry(name, position))
      case Some(entry) => entry.compiled
    }
  }

  /**
    * Registers the given entry with the scope. If it is already registered in the current scope, an
    * "already declared" error is returned instead.
    */
  def register(name: String, entry: A)(implicit position: Position): Verification = {
    if (local(name).isDefined) {
      Compilation.fail(alreadyDeclared(name, position))
    } else {
      add(name, entry)
      Verification.succeed
    }
  }

  /**
    * The label of the entry this scope contains, such as a variable or a type. The label is used when "unknown entry"
    * and "already declared" errors are created. You may override this to generate better error messages.
    */
  protected def entryLabel: String = "entry"

  /**
    * Creates an "unknown entry" error.
    */
  private def unknownEntry(name: String, position: Position): Feedback.Error = UnknownEntry(entryLabel, name, position)

  /**
    * Creates an "already declared" error.
    */
  private def alreadyDeclared(name: String, position: Position): Feedback.Error = AlreadyDeclared(entryLabel, name, position)

}

abstract class BasicScope[A](override val parent: Option[Scope[A]]) extends Scope[A] {
  protected val entries: mutable.Map[String, A] = new mutable.HashMap()

  override protected def local(name: String): Option[A] = entries.get(name)

  override protected def add(name: String, entry: A): Unit = {
    if (entries.contains(name)) {
      throw CompilationException(s"The $entryLabel $name is already defined in the local scope and cannot be redefined.")
    }
    entries.put(name, entry)
  }
}

object Scope {
  case class AlreadyDeclared(label: String, name: String, override val position: Position) extends Feedback.Error(position) {
    override def message = s"The $label $name has already been declared in the current scope."
  }

  case class UnknownEntry(label: String, name: String, override val position: Position) extends Feedback.Error(position) {
    override def message = s"The $label $name does not exist in the current scope."
  }
}
