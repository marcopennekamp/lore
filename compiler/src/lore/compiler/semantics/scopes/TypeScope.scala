package lore.compiler.semantics.scopes

import lore.compiler.core.Position
import lore.compiler.feedback.{Reporter, ScopeFeedback}
import lore.compiler.semantics.NamePath
import lore.compiler.semantics.modules.ModuleDefinition
import lore.compiler.types._
import lore.compiler.utils.CollectionExtensions.OptionExtension

/**
  * A scope that provides access to type schemas.
  */
trait TypeScope extends Scope[NamedSchema] {

  /**
    * Resolves a schema either by simple name, or by a more complex name path. A name path must be comprised of 1 to N
    * module names, followed by a type name as the last segment. Types cannot contain types and thus cannot be earlier
    * segments in a name path.
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
  def resolve(namePath: NamePath, position: Position)(implicit bindingScope: BindingScope, reporter: Reporter): Option[NamedSchema] = {
    // If the name path only contains a single segment, we don't need to resolve any module paths.
    if (!namePath.isMultiple) {
      return resolve(namePath.simpleName, position)
    }

    // To get the correct binding which we can jump off of, we have to search with the name path's head name.
    bindingScope.get(namePath.headName) match {
      case Some(binding) => binding match {
        case module: ModuleDefinition =>
          val typePath = module.name ++ namePath.tail
          resolveAbsolute(typePath, position)

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
    * Resolves the type identified by the <b>absolute</b> name path. By default, this function passes the ball to its
    * parent scope.
    */
  protected def resolveAbsolute(absolutePath: NamePath, position: Position)(implicit reporter: Reporter): Option[NamedSchema] = {
    optionalParent.filterType[TypeScope].flatMap(_.resolveAbsolute(absolutePath, position))
  }

  override def entryLabel: String = "type"
}

case class ImmutableTypeScope(
  entries: Map[String, NamedSchema],
  override val optionalParent: Option[TypeScope],
) extends ImmutableScope[NamedSchema] with TypeScope

object ImmutableTypeScope {
  def from(schemas: Vector[NamedSchema], parent: TypeScope): ImmutableTypeScope = {
    ImmutableTypeScope(schemas.map(tpe => (tpe.name.simpleName, tpe)).toMap, Some(parent))
  }
}
