package lore.lsp.index

import scala.collection.{immutable, mutable}

/**
  * Memorizes all index declarations contained in all fragments so that we can remove them with little effort when a
  * fragment changes.
  */
class FragmentDeclarationsCache {

  private val fragmentIndexDeclarations: mutable.Map[String, Set[IndexDeclaration]] = mutable.HashMap()

  def get(fragmentUri: String): Set[IndexDeclaration] = fragmentIndexDeclarations.getOrElse(fragmentUri, Set.empty)

  /**
    * Ensure that the given declaration is contained in the caches of all fragments it is located in.
    */
  def update(declaration: IndexDeclaration): Unit = {
    distinctUris(declaration).foreach { fragmentUri =>
      fragmentIndexDeclarations.updateWith(fragmentUri) {
        case Some(declarations) => Some(declarations + declaration)
        case None => Some(immutable.HashSet(declaration))
      }
    }
  }

  /**
    * Remove the declaration from the cache of the given fragment.
    */
  def remove(declaration: IndexDeclaration, fragmentUri: String): Unit = {
    fragmentIndexDeclarations.updateWith(fragmentUri) {
      case Some(declarations) => Some(declarations - declaration)
      case None => None
    }
  }

  /**
    * Remove the declaration from all fragment caches.
    */
  def remove(declaration: IndexDeclaration): Unit = {
    distinctUris(declaration).foreach(remove(declaration, _))
  }

  private def distinctUris(declaration: IndexDeclaration): Vector[String] = declaration.locations.map(_.getUri).distinct

}
