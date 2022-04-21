package lore.compiler.semantics.functions

import lore.compiler.core.{CompilationException, Position, Positioned}
import lore.compiler.feedback.{Feedback, Reporter}
import lore.compiler.semantics.expressions.Expression
import lore.compiler.semantics.functions.FunctionDefinition.CannotInstantiateFunction
import lore.compiler.semantics.modules.LocalModule
import lore.compiler.semantics.scopes.{BindingScope, FunctionBindingScope, ImmutableTypeScope, TypeScope}
import lore.compiler.semantics.{NamePath, Registry}
import lore.compiler.syntax.ExprNode
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
  val bodyNode: Option[ExprNode],
  val localModule: LocalModule,
) extends Positioned {
  override val position: Position = signature.position
  override def toString = s"${if (isAbstract) "abstract " else ""}$name(${signature.parameters.mkString(", ")})"

  val name: NamePath = signature.name
  val typeParameters: Vector[TypeVariable] = signature.typeParameters
  val isAbstract: Boolean = bodyNode.isEmpty
  val isPolymorphic: Boolean = signature.isPolymorphic
  val isMonomorphic: Boolean = signature.isMonomorphic

  /**
    * The multi-function this function is a part of. This is immediately initialized after the multi-function is created.
    */
  var multiFunction: MultiFunctionDefinition = _

  /**
    * This is a variable because it may be transformed during the course of the compilation.
    */
  var body: Option[Expression] = None

  def getTypeScope(implicit registry: Registry): TypeScope = FunctionDefinition.typeScope(typeParameters, registry.getTypeScope(localModule))
  def getBindingScope(implicit registry: Registry): BindingScope = FunctionBindingScope(signature, registry.getBindingScope(localModule))

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
    * A function instance that is effectively equal to this monomorphic function definition, as a monomorphic function
    * doesn't have any type variables that would need to be substituted.
    */
  lazy val monomorphicInstance: FunctionInstance = {
    if (!isMonomorphic) {
      throw CompilationException(s"The function instance $signature cannot be instantiated monomorphically, because it is not monomorphic.")
    }
    FunctionInstance(this, signature)
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
