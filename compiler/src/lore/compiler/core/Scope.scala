package lore.compiler.core

import lore.compiler.core.Compilation.{C, Verification}
import lore.compiler.core.Scope.{AlreadyDeclared, UnknownEntry}
import lore.compiler.core.feedback.{Error, Position}

import scala.collection.mutable

/**
  * A hierarchical scope resolving entries of some type A by name.
  */
trait Scope[A <: Scope.Entry] {
  // TODO: Turn the position parameters into implicit ones. If we can pass the fragment as an implicit, we should pass
  //       the position as an implicit as well.

  /**
    * Fetches the entry with the given name from the closest scope.
    */
  def get(name: String): Option[A]

  /**
    * Adds the given entry to the scope.
    */
  protected def add(entry: A): Unit

  /**
    * Resolves the entry with the given name from the closest scope. If it cannot be found, we return a
    * compilation error.
    */
  def resolve(name: String)(implicit position: Position): C[A] = {
    get(name) match {
      case Some(entry) => Compilation.succeed(entry)
      case None => Compilation.fail(unknownEntry(name))
    }
  }

  /**
    * Registers the given entry with the scope. If it is already registered in the CURRENT scope, an
    * "already declared" error is returned instead.
    */
  def register(entry: A)(implicit position: Position): Verification = {
    if (get(entry.name).isDefined) {
      Compilation.fail(alreadyDeclared(entry.name))
    } else {
      add(entry)
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

abstract class BasicScope[A <: Scope.Entry](val parent: Option[Scope[A]]) extends Scope[A] {
  protected val entries: mutable.Map[String, A] = new mutable.HashMap()
  override def get(name: String): Option[A] = parent.flatMap(_.get(name)).orElse(entries.get(name))
  override protected def add(entry: A): Unit = {
    assert(!entries.contains(entry.name))
    entries.put(entry.name, entry)
  }
}

object Scope {
  trait Entry {
    def name: String
  }

  case class AlreadyDeclared(name: String)(implicit position: Position) extends Error(position) {
    override def message = s"An entry '$name' has already been declared in the current scope."
  }

  case class UnknownEntry(name: String)(implicit position: Position) extends Error(position) {
    override def message = s"The current scope does not know an entry '$name'."
  }
}
