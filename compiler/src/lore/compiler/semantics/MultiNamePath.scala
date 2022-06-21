package lore.compiler.semantics

import lore.compiler.core.CompilationException

/**
  * A collection of name paths that have been resolved from the same simple name. Multi name paths usually need to be
  * disambiguated at compile time in some way so that the underlying binding can be used.
  *
  * Multi name paths are built during name resolution for all bindings due to the possibility that a binding might be
  * multi-referable.
  *
  * TODO (multi-import): Move this to the `modules` package if multi name paths are only constructed and returned by
  *                      local modules.
  *
  * @param local Name paths that refer to a local definition or import.
  * @param global Name paths that refer to a global definition.
  */
case class MultiNamePath(
  local: Set[NamePath],
  global: Set[NamePath],
  bindingKind: BindingKind,
) {
  val namePaths: Set[NamePath] = local ++ global
  verify()

  /**
    * If the binding is single-referable, returns the multi name path's only name path. That there will only be a
    * single name path is enforced by [[verify]]. If the binding is multi-referable, a [[CompilationException]] will be
    * thrown.
    */
  def singlePath: NamePath = {
    if (bindingKind.isSingleReferable) namePaths.head
    else throw CompilationException("`MultiNamePath.singlePath` can only be used with single-referable bindings." +
      s" Binding kind: $bindingKind.")
  }

  /**
    * Whether this multi name path is compatible with `other`, meaning that their binding kinds agree so that they
    * could be merged.
    */
  def isCompatibleWith(other: MultiNamePath): Boolean = bindingKind == other.bindingKind

  /**
    * Concatenates this multi name path with `other`. The bindings kinds must agree.
    */
  def ++(other: MultiNamePath): MultiNamePath = {
    if (!this.isCompatibleWith(other)) {
      throw CompilationException(s"Binding kinds must agree for MultiNamePath concatenation. Own binding kind:" +
        s" $bindingKind. Other binding kind: ${other.bindingKind}.")
    }
    MultiNamePath(local ++ other.local, global ++ other.global, bindingKind)
  }

  /**
    * Creates a new multi name path with `path` added as a local name path.
    */
  def addLocal(namePath: NamePath): MultiNamePath = MultiNamePath(local + namePath, global, bindingKind)

  private def verify(): Unit = {
    if (namePaths.isEmpty) {
      throw CompilationException("MultiNamePaths must contain at least one name path.")
    }

    if (bindingKind.isSingleReferable && namePaths.size > 1) {
      throw CompilationException(s"MultiNamePaths for single-referable bindings must be instantiated with a single name" +
        s" path. Binding kind: $bindingKind.")
    }

    if (namePaths.map(_.simpleName).size > 1) {
      throw CompilationException(s"All name paths of a MultiNamePath must have the same simple name. Name paths:" +
        s" ${namePaths.mkString(", ")}.")
    }
  }
}
