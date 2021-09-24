package lore.compiler.semantics.scopes

import lore.compiler.core.Position
import lore.compiler.feedback.Reporter
import lore.compiler.semantics.NamePath
import lore.compiler.semantics.functions.FunctionSignature

/**
  * A scope that provides access to bindings (variables, multi-functions, struct constructors, modules, etc.).
  */
trait BindingScope extends Scope[Binding] {
  def resolveStatic(namePath: NamePath, position: Position)(implicit reporter: Reporter): Option[Binding] = {
    resolveStatic(namePath, this, position)
  }

  override def entryLabel: String = "variable, multi-function, struct constructor, or module"
}

/**
  * The root binding scope of a function, containing parameter bindings.
  */
case class FunctionBindingScope(signature: FunctionSignature, parent: BindingScope) extends ImmutableScope[Binding] with BindingScope {
  override protected val optionalParent: Option[Scope[Binding]] = Some(parent)
  override protected val entries: Map[String, Binding] = signature.namedParameters.map(p => p.name -> p.asVariable).toMap
}

/**
  * A scope opened by a block, containing local variable bindings.
  */
class BlockBindingScope(parent: BindingScope) extends MutableScope[Binding] with BindingScope {
  override protected def optionalParent: Option[Scope[Binding]] = Some(parent)
  def register(variable: LocalVariable, position: Position)(implicit reporter: Reporter): Unit = {
    super.register(variable.name, variable, position)
  }
}
