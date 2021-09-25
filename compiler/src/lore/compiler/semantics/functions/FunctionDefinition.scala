package lore.compiler.semantics.functions

import lore.compiler.core.{Position, Positioned}
import lore.compiler.feedback.{Feedback, Reporter}
import lore.compiler.semantics.{NamePath, Registry}
import lore.compiler.semantics.expressions.Expression
import lore.compiler.semantics.functions.FunctionDefinition.CannotInstantiateFunction
import lore.compiler.semantics.modules.LocalModule
import lore.compiler.semantics.scopes.{BindingScope, FunctionBindingScope, ImmutableTypeScope, TypeScope}
import lore.compiler.syntax.ExprNode
import lore.compiler.target.TargetRepresentable
import lore.compiler.transpilation.RuntimeNames
import lore.compiler.types.{Fit, Type, TypeVariable}

/**
  * A definition of a single function as part of a larger multi-function.
  *
  * Function definition equality is always reference equality, as we create exactly one function definition
  * for every defined function.
  *
  * The position is restricted to the function's name for better error highlighting and index building.
  */
class FunctionDefinition(
  val signature: FunctionSignature,
  val typeParameters: Vector[TypeVariable],
  val bodyNode: Option[ExprNode],
  val localModule: LocalModule,
) extends Positioned with TargetRepresentable {
  override val position: Position = signature.position
  override def toString = s"${if (isAbstract) "abstract " else ""}$name(${signature.parameters.mkString(", ")})"

  val name: NamePath = signature.name
  val isAbstract: Boolean = bodyNode.isEmpty
  val isPolymorphic: Boolean = signature.isPolymorphic
  val isMonomorphic: Boolean = signature.isMonomorphic

  def getTypeScope(implicit registry: Registry): TypeScope = FunctionDefinition.typeScope(typeParameters, registry.getTypeScope(localModule))
  def getBindingScope(implicit registry: Registry): BindingScope = FunctionBindingScope(signature, registry.getBindingScope(localModule))

  /**
    * This is a variable because it may be transformed during the course of the compilation.
    */
  var body: Option[Expression] = _

  /**
    * Attempts to instantiate the function definition with the given argument type. If this is not possible, reports a
    * "cannot instantiate function" error.
    */
  def instantiate(argumentType: Type)(implicit reporter: Reporter): Option[FunctionInstance] = {
    val option = Fit
      .assignments(argumentType, signature.inputType)
      .map(assignments => FunctionInstance(this, signature.substitute(assignments)))

    if (option.isEmpty) {
      reporter.error(CannotInstantiateFunction(this, argumentType))
    }
    option
  }

  /**
    * The run-time name of a function incorporates a hash of the function's input type, which makes it practically
    * unique for each function definition. This is similar to C++'s name mangling, though with an element of highly
    * unlikely collision. This approach is preferable to just giving the function an index or a UUID, because a stable
    * identifier allows us to reconstruct the function's name, which will be especially useful between compilation
    * passes.
    */
  lazy val runtimeName: String = {
    val runtimeName = RuntimeNames.namePath(name)
    val id = Type.stableIdentifier(signature.inputType.elements)
    s"$runtimeName$$$id"
  }
}

object FunctionDefinition {
  case class CannotInstantiateFunction(definition: FunctionDefinition, argumentType: Type) extends Feedback.Error(definition) {
    override def message = s"The function definition $definition cannot be instantiated from argument type $argumentType."
  }

  /**
    * Creates an immutable type scope that allows access to the function's type parameters.
    */
  def typeScope(typeParameters: Vector[TypeVariable], parentScope: TypeScope): TypeScope = ImmutableTypeScope.from(typeParameters, parentScope)
}
