package lore.compiler.semantics.functions

import lore.compiler.core.{CompilationException, Position, Positioned}
import lore.compiler.semantics.definitions.HasLocalModule
import lore.compiler.semantics.expressions.Expression
import lore.compiler.semantics.scopes.{FunctionTermScope, ImmutableTypeScope, TermScope, TypeScope}
import lore.compiler.semantics.{NamePath, Registry}
import lore.compiler.syntax.DeclNode.FunctionNode
import lore.compiler.types.{Fit, TupleType, TypeVariable}
import lore.compiler.utils.Once

/**
  * A definition of a single function as part of a larger multi-function.
  *
  * Function definition equality is always reference equality, as we create exactly one function definition
  * for every defined function.
  *
  * The position is restricted to the function's name for better error highlighting and index building.
  *
  * TODO (multi-import): Make this a FunctionLike?
  */
class FunctionDefinition(
  val signature: FunctionSignature,
  val node: FunctionNode,
  val multiFunction: MultiFunctionDefinition, // TODO (multi-import): Make this the first property.
) extends Positioned with HasLocalModule {
  val name: NamePath = signature.name
  override val position: Position = signature.position
  override def toString = s"${if (isAbstract) "abstract " else ""}$name(${signature.parameters.mkString(", ")})"

  val typeParameters: Vector[TypeVariable] = signature.typeParameters
  val isAbstract: Boolean = node.body.isEmpty
  val isPolymorphic: Boolean = signature.isPolymorphic
  val isMonomorphic: Boolean = signature.isMonomorphic

  val body: Once[Option[Expression]] = new Once

  def getTypeScope(implicit registry: Registry): TypeScope = {
    FunctionDefinition.typeScope(typeParameters, registry.getTypeScope(localModule))
  }

  def getTermScope(implicit registry: Registry): TermScope = {
    FunctionTermScope(signature, registry.getTermScope(localModule))
  }

  /**
    * Attempts to instantiate the function definition with the given argument type. Returns `None` if the function
    * cannot be instantiated.
    */
  def instantiate(argumentType: TupleType): Option[FunctionInstance] = {
    Fit
      .fitsAssignments(argumentType, signature.inputType)
      .map(assignments => FunctionInstance(this, assignments))
  }

  /**
    * A function instance that is effectively equal to this monomorphic function definition, as a monomorphic function
    * doesn't have any type variables that would need to be substituted.
    */
  lazy val monomorphicInstance: FunctionInstance = {
    if (!isMonomorphic) {
      throw CompilationException(s"The function instance $signature cannot be instantiated monomorphically, because it" +
        s" is not monomorphic.")
    }
    FunctionInstance(this, Map.empty)
  }
}

object FunctionDefinition {
  /**
    * Creates an immutable type scope that allows access to the function's type parameters.
    */
  def typeScope(typeParameters: Vector[TypeVariable], parentScope: TypeScope): TypeScope = ImmutableTypeScope.from(typeParameters, parentScope)
}
