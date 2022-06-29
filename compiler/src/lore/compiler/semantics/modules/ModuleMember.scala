package lore.compiler.semantics.modules

import lore.compiler.core.{CompilationException, Position, Positioned}
import lore.compiler.semantics.bindings.{StructBinding, TermBinding}
import lore.compiler.semantics.functions.MultiFunctionDefinition
import lore.compiler.semantics.specs.SpecDefinition
import lore.compiler.semantics.variables.GlobalVariableDefinition
import lore.compiler.semantics.{BindingKind, NamePath}
import lore.compiler.syntax.DeclNode.{FunctionNode, GlobalVariableNode, SpecNode}
import lore.compiler.syntax.TypeDeclNode
import lore.compiler.types.NamedSchema
import lore.compiler.utils.Once

/**
  * A member of a module. Module members are owned by global modules and referenced by local modules.
  */
sealed trait ModuleMember extends Positioned {
  def owner: GlobalModule
  def positions: Iterable[Position]

  override def position: Position = positions.headOption.getOrElse(Position.unknown)
}

/**
  * A named module member that represents either a type or a term based on the binding kind.
  *
  * Module members are first created during global module resolution as "empty shells" without their resolved binding
  * attached. This allows local modules to reference and expose module members before bindings are resolved, which is
  * needed for name resolution. Finally, each binding is resolved in the appropriate order and added to the module
  * member.
  *
  * All kinds of module members must be classes instead of case classes because they're referenced by local modules.
  * There should be exactly one module member instance per actual module member.
  */
sealed trait BindingModuleMember extends ModuleMember {
  def simpleName: String
  def bindingKind: BindingKind

  lazy val name: NamePath = owner.name + simpleName
}

sealed trait TypeModuleMember extends BindingModuleMember {
  /**
    * The [[NamedSchema]] of the type module member, once it has been resolved.
    *
    * Caution: The resolution phase does not guarantee that all schemas are resolved. For example, a type with cyclic
    * inheritance will not be added to the schema resolution order and thereby also not resolved.
    */
  def schema: Once[NamedSchema]

  override def bindingKind: BindingKind = BindingKind.Type
}

// TODO (multi-import): This includes any kind of TypeDeclNode, but conflicts by name with DeclaredTypes... Not very
//                      pretty. DeclaredTypes should actually be DataTypes or something in that direction, so maybe we
//                      should rename declared types, even despite it being a huge undertaking.
class DeclaredTypeModuleMember(
  val declNode: TypeDeclNode,
  override val owner: GlobalModule,
) extends TypeModuleMember {
  override val schema: Once[NamedSchema] = new Once

  override def simpleName: String = declNode.simpleName
  override def bindingKind: BindingKind = BindingKind.Type
  override val positions: Vector[Position] = Vector(declNode.position)
}

class BuiltinTypeModuleMember(
  tpe: NamedSchema,
  override val owner: GlobalModule,
) extends TypeModuleMember {
  override val schema: Once[NamedSchema] = Once(tpe)
  override def simpleName: String = tpe.name.simpleName
  override def positions: Vector[Position] = Vector.empty
}

sealed trait TermModuleMember extends BindingModuleMember {
  def binding: TermBinding
}

// TODO (multi-import): `owner` can technically be found out by taking the parent of `globalModule´.
class ModuleModuleMember(
  val globalModule: GlobalModule,
  override val owner: GlobalModule,
) extends TermModuleMember {
  override def simpleName: String = globalModule.name.simpleName
  override def binding: TermBinding = globalModule
  override def bindingKind: BindingKind = BindingKind.Module
  override def positions: Vector[Position] = globalModule.positions
}

/**
  * @param typeModuleMember The struct type member that this struct binding references.
  */
class StructModuleMember(
  val typeModuleMember: DeclaredTypeModuleMember,
  override val owner: GlobalModule,
) extends TermModuleMember {
  val structBinding: Once[StructBinding] = new Once

  /**
    * The module member's companion module is used as a fallback in case [[structBinding]] has not yet been set. See
    * `test/language/modules/companion_cycle.lore` for a motivating example.
    */
  var companionModule: Option[GlobalModule] = None

  override def simpleName: String = typeModuleMember.simpleName
  override def binding: TermBinding = companionModule match {
    case Some(companionModule) => if (structBinding.isAssigned) structBinding.value else companionModule
    case None => structBinding.value
  }
  override def bindingKind: BindingKind = BindingKind.Struct
  override def positions: Vector[Position] = typeModuleMember.positions
}

class GlobalVariableModuleMember(
  val declNode: GlobalVariableNode,
  override val owner: GlobalModule,
) extends TermModuleMember {
  val globalVariable: Once[GlobalVariableDefinition] = new Once

  override def simpleName: String = declNode.simpleName
  override def binding: TermBinding = globalVariable.value
  override def bindingKind: BindingKind = BindingKind.GlobalVariable
  override def positions: Vector[Position] = Vector(declNode.position)
}

/**
  * @param declNode A function node encountered during module resolution. This is not necessarily the only function
  *                 node associated with the multi-function.
  */
class MultiFunctionModuleMember(
  declNode: FunctionNode,
  override val owner: GlobalModule,
) extends TermModuleMember {
  val multiFunction: Once[MultiFunctionDefinition] = new Once
  private var _functionNodes: Vector[FunctionNode] = Vector(declNode)

  def functionNodes: Vector[FunctionNode] = _functionNodes

  def addFunctionNode(node: FunctionNode): Unit = {
    if (multiFunction.isAssigned) {
      throw CompilationException(s"Cannot add a function node to multi-function module member `$name`: the" +
        s" multi-function has already been initialized.")
    }
    _functionNodes :+= node
  }

  override def simpleName: String = declNode.simpleName
  override def binding: TermBinding = multiFunction.value
  override def bindingKind: BindingKind = BindingKind.MultiFunction
  override def positions: Iterable[Position] = _functionNodes.view.map(_.position) // Lazy map due to view.
}

class SpecModuleMember(
  val declNode: SpecNode,
  override val owner: GlobalModule,
) extends ModuleMember {
  val spec: Once[SpecDefinition] = new Once

  override val positions: Iterable[Position] = Vector(declNode.position)
}
