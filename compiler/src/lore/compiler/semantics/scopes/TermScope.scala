package lore.compiler.semantics.scopes

import lore.compiler.core.Position
import lore.compiler.feedback.Reporter
import lore.compiler.semantics.NamePath
import lore.compiler.semantics.bindings.{LocalVariable, TermBinding}
import lore.compiler.semantics.functions.FunctionSignature

/**
  * A scope that provides access to terms (variables, multi-functions, struct constructors, modules, etc.).
  */
trait TermScope extends Scope[TermBinding] {
  def resolveStatic(namePath: NamePath, position: Position)(implicit reporter: Reporter): Option[TermBinding] = {
    resolveStatic(namePath, this, position)
  }

  override def entryLabel: String = "binding"
}

/**
  * The root term scope of a function, containing parameter bindings.
  */
case class FunctionTermScope(signature: FunctionSignature, parent: TermScope) extends ImmutableScope[TermBinding] with TermScope {
  override protected val optionalParent: Option[Scope[TermBinding]] = Some(parent)
  override protected val entries: Map[String, TermBinding] = signature.namedParameters.map(p => p.name -> p.asVariable).toMap
}

/**
  * A scope opened by a block, containing local variable bindings.
  */
class BlockTermScope(parent: TermScope) extends MutableScope[TermBinding] with TermScope {
  override protected def optionalParent: Option[Scope[TermBinding]] = Some(parent)
  def register(variable: LocalVariable, position: Position)(implicit reporter: Reporter): Unit = {
    super.register(variable.name, variable, position)
  }
}
