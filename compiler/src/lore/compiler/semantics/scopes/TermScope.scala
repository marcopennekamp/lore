package lore.compiler.semantics.scopes

import lore.compiler.core.Position
import lore.compiler.feedback.Reporter
import lore.compiler.semantics.NamePath
import lore.compiler.semantics.bindings.{LocalVariable, StructBinding, TermBinding}
import lore.compiler.semantics.functions.FunctionSignature
import lore.compiler.semantics.modules.GlobalModule

/**
  * A scope that provides access to terms (variables, multi-functions, struct constructors, modules, etc.).
  */
trait TermScope extends Scope[TermBinding] {
  def resolveStatic(namePath: NamePath, position: Position)(implicit reporter: Reporter): Option[TermBinding] = {
    resolveStatic(namePath, this, position)
  }

  /**
    * Fetches a module or companion module `name` from the closest scope, or `None` if `name` doesn't exist or isn't a
    * module.
    */
  def getModule(name: String): Option[GlobalModule] = get(name).flatMap {
    case module: GlobalModule => Some(module)
    case structBinding: StructBinding => structBinding.companionModule
    case _ => None
  }

  override protected def optionalParent: Option[TermScope] = None
  override def entryLabel: String = "binding"
}

/**
  * The root term scope of a function, containing parameter bindings.
  */
case class FunctionTermScope(signature: FunctionSignature, parent: TermScope) extends ImmutableScope[TermBinding] with TermScope {
  override protected val optionalParent: Option[TermScope] = Some(parent)
  override protected val entries: Map[String, TermBinding] = signature.namedParameters.map(p => p.name -> p.asVariable).toMap
}

/**
  * A scope opened by a block, containing local variable bindings.
  */
class BlockTermScope(parent: TermScope) extends MutableScope[TermBinding] with TermScope {
  override protected def optionalParent: Option[TermScope] = Some(parent)
  def register(variable: LocalVariable, position: Position)(implicit reporter: Reporter): Unit = {
    super.register(variable.name, variable, position)
  }
}
