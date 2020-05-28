package lore.compiler.core

import lore.compiler.core.Compilation.{C, Verification}
import lore.compiler.core.Scope.{AlreadyDeclared, UnknownEntry}
import lore.compiler.feedback.{Error, Position}

import scala.collection.mutable

/**
  * A hierarchical scope resolving entries of some type A by name.
  */
abstract class Scope[A <: Scope.Entry](val parent: Option[Scope[A]]) {
  protected val entries: mutable.Map[String, A] = new mutable.HashMap()

  /**
    * Fetches the entry with the given name from the closest scope.
    */
  def get(name: String): Option[A] = parent.flatMap(_.get(name)).orElse(entries.get(name))

  /**
    * Fetches the entry with the given name from the closest scope. If it cannot be found, we return a
    * compilation error.
    */
  def entry(name: String, position: Position): C[A] = {
    get(name) match {
      case Some(entry) => Compilation.succeed(entry)
      case None => Compilation.fail(UnknownEntry(name, position))
    }
  }

  /**
    * Adds the given entry to the scope.
    */
  protected def add(entry: A): Unit = {
    assert(!entries.contains(entry.name))
    entries.put(entry.name, entry)
  }

  /**
    * Registers the given entry with the scope. If it is already registered in the CURRENT scope, an
    * "already declared" error is returned instead.
    */
  def register(entry: A, position: Position): Verification = {
    if (entries.contains(entry.name)) {
      Compilation.fail(AlreadyDeclared(entry.name, position))
    } else {
      add(entry)
      Verification.succeed
    }
  }
}

object Scope {
  trait Entry {
    def name: String
  }

  case class AlreadyDeclared(name: String, pos: Position) extends Error(pos) {
    override def message = s"An entry '$name' has already been declared in the current scope."
  }

  case class UnknownEntry(name: String, pos: Position) extends Error(pos) {
    override def message = s"The current scope does not know an entry '$name'."
  }
}
