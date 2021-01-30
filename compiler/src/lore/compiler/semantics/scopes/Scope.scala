package lore.compiler.semantics.scopes

import lore.compiler.core.Compilation.{ToCompilationExtension, Verification}
import lore.compiler.core.{Compilation, CompilationException, Error, Position}
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
      case None => Compilation.fail(unknownEntry(name))
      case Some(entry) => entry.compiled
    }
  }

  /**
    * Registers the given entry with the scope. If it is already registered in the current scope, an
    * "already declared" error is returned instead.
    */
  def register(name: String, entry: A)(implicit position: Position): Verification = {
    if (local(name).isDefined) {
      Compilation.fail(alreadyDeclared(name))
    } else {
      add(name, entry)
      Verification.succeed
    }
  }

  /**
    * Creates an "unknown entry" error. You may override this to provide better error messages.
    */
  protected def unknownEntry(name: String)(implicit position: Position): Error = UnknownEntry(name)

  /**
    * Creates an "already declared" error. You may override this to provide better error messages.
    */
  protected def alreadyDeclared(name: String)(implicit position: Position): Error = AlreadyDeclared(name)

}

abstract class BasicScope[A](override val parent: Option[Scope[A]]) extends Scope[A] {
  protected val entries: mutable.Map[String, A] = new mutable.HashMap()

  override protected def local(name: String): Option[A] = entries.get(name)

  override protected def add(name: String, entry: A): Unit = {
    if (entries.contains(name)) {
      throw CompilationException(s"An entry '$name' is already defined in the local scope and cannot be redefined.")
    }
    entries.put(name, entry)
  }
}

object Scope {
  // TODO: Specify what kind of entry is already declared/unknown. For example: "The current scope does not know a
  //       type X."

  case class AlreadyDeclared(name: String)(implicit position: Position) extends Error(position) {
    override def message = s"An entry '$name' has already been declared in the current scope."
  }

  case class UnknownEntry(name: String)(implicit position: Position) extends Error(position) {
    override def message = s"The current scope does not know an entry '$name'."
  }
}
