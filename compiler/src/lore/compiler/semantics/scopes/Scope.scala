package lore.compiler.semantics.scopes

import lore.compiler.core.Position
import lore.compiler.feedback.{Reporter, ScopeFeedback}
import lore.compiler.semantics.NamePath
import lore.compiler.semantics.modules.GlobalModule
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
    * Fetches a member of the global scope identified by the <b>absolute</b> name path.
    */
  def global(absolutePath: NamePath): Option[A] = {
    optionalParent.flatMap(_.global(absolutePath))
  }

  /**
    * Resolves a member of the global scope identified by the <b>absolute</b> name path. By default, this function
    * delegates to its parent scope.
    */
  def resolveGlobal(absolutePath: NamePath, position: Position)(implicit reporter: Reporter): Option[A] = {
    global(absolutePath).ifEmpty(reporter.error(ScopeFeedback.UnknownEntry(entryLabel, absolutePath.toString, position)))
  }

  /**
    * Resolves an entry that is either plainly named with a simple name, or belongs to a module. In the latter case,
    * the head segment of the name path must refer to a module name.
    *
    * This function should be used in cases where members of other bindings (such as struct properties) should be
    * explicitly excluded. In other words, only <i>static</i> bindings and types are considered.
    *
    * To use this function from outside the scope, please use the `resolveStatic` functions in BindingScope and
    * TypeScope.
    *
    * To resolve the correct module, this function requires a [[BindingScope]]. Supplying a LocalModule is not
    * sufficient, because module names are shadowed by e.g. local variable names. Code such as this should not compile:
    *
    * <pre>
    * // Earlier: Module `foo` contains type `Bar`.
    * let foo = 5
    * let bar: foo.Bar = foo.Bar()
    * </pre>
    *
    * `foo` should refer to the local variable here, not the module. Hence, using a LocalModule is not sufficient for
    * module name resolution.
    */
  protected def resolveStatic(namePath: NamePath, bindingScope: BindingScope, position: Position)(implicit reporter: Reporter): Option[A] = {
    // If the name path only contains a single segment, we don't need to resolve any module paths.
    if (!namePath.isMultiple) {
      return resolve(namePath.simpleName, position)
    }

    // To get the correct binding which we can jump off of, we have to search with the name path's head name.
    bindingScope.get(namePath.headName) match {
      case Some(binding) => binding match {
        case module: GlobalModule => resolveGlobal(module.name ++ namePath.tail, position)
        case _ =>
          reporter.error(ScopeFeedback.ModuleExpected(namePath.headName, position))
          None
      }

      case None =>
        reporter.error(ScopeFeedback.ModuleNotFound(namePath.headName, position))
        None
    }
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
