package lore.compiler.core

import lore.compiler.ast.Node
import lore.compiler.core.Compilation.{C, Verification}
import lore.compiler.core.Scope.{AlreadyDeclared, UnknownEntry}
import lore.compiler.feedback.{Error, Position}

import scala.collection.mutable

/**
  * A hierarchical scope resolving entries of some type A by name.
  */
trait Scope[A <: lore.core.Scope.Entry] {
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
  def resolve(name: String, position: Position): C[A] = {
    get(name) match {
      case Some(entry) => Compilation.succeed(entry)
      case None => Compilation.fail(unknownEntry(name, position))
    }
  }

  /**
    * Resolves the entry with the given name from the closest scope. If it cannot be found, we return a
    * compilation error.
    */
  def resolve(name: String, associatedNode: Node)(implicit fragment: Fragment): C[A] = {
    resolve(name, associatedNode.position)
  }

  /**
    * Registers the given entry with the scope. If it is already registered in the CURRENT scope, an
    * "already declared" error is returned instead.
    */
  def register(entry: A, position: Position): Verification = {
    if (get(entry.name).isDefined) {
      Compilation.fail(alreadyDeclared(entry.name, position))
    } else {
      add(entry)
      Verification.succeed
    }
  }

  /**
    * Creates an "unknown entry" error. You may override this to provide better error messages.
    */
  protected def unknownEntry(name: String, position: Position): Error = UnknownEntry(name, position)

  /**
    * Creates an "already declared" error. You may override this to provide better error messages.
    */
  protected def alreadyDeclared(name: String, position: Position): Error = AlreadyDeclared(name, position)
}

abstract class BasicScope[A <: lore.core.Scope.Entry](val parent: Option[Scope[A]]) extends Scope[A] {
  protected val entries: mutable.Map[String, A] = new mutable.HashMap()
  override def get(name: String): Option[A] = parent.flatMap(_.get(name)).orElse(entries.get(name))
  override protected def add(entry: A): Unit = {
    assert(!entries.contains(entry.name))
    entries.put(entry.name, entry)
  }
}

object Scope {
  case class AlreadyDeclared(name: String, pos: Position) extends Error(pos) {
    override def message = s"An entry '$name' has already been declared in the current scope."
  }

  case class UnknownEntry(name: String, pos: Position) extends Error(pos) {
    override def message = s"The current scope does not know an entry '$name'."
  }
}
