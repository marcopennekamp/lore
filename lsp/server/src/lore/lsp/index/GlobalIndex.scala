package lore.lsp.index

import org.eclipse.lsp4j.Location

import scala.collection.mutable

class GlobalIndex {

  private val typeDeclarations: mutable.Map[String, IndexTypeDeclaration] = mutable.HashMap()
  private val bindingDeclarations: mutable.Map[String, IndexBindingDeclaration] = mutable.HashMap()

  val fragmentDeclarationsCache = new FragmentDeclarationsCache

  def getTypeDeclaration(name: String): Option[IndexTypeDeclaration] = typeDeclarations.get(name)

  def updateTypeDeclaration(name: String, location: Location): IndexTypeDeclaration = {
    // If the type is redeclared in a different fragment, we want to remove it from the old fragment. This requires us
    // to update the fragment index declarations accordingly.
    getTypeDeclaration(name).foreach { declaration =>
      if (declaration.location.getUri != location.getUri) {
        fragmentDeclarationsCache.remove(declaration)
      }
    }

    val declaration = typeDeclarations.getOrElseUpdate(name, new IndexTypeDeclaration(name, location))
    declaration.updateLocation(location)
    fragmentDeclarationsCache.update(declaration)
    declaration
  }

  def removeTypeDeclaration(name: String): Unit = {
    typeDeclarations.remove(name).foreach(fragmentDeclarationsCache.remove(_))
  }

  def getBindingDeclaration(name: String): Option[IndexBindingDeclaration] = bindingDeclarations.get(name)

  /**
    * Replaces all current locations of the binding declaration with the given locations.
    */
  def replaceBindingDeclaration(name: String, locations: Vector[Location]): IndexBindingDeclaration = {
    // If the binding is already declared, remove it from all fragment caches temporarily.
    getBindingDeclaration(name).foreach(fragmentDeclarationsCache.remove(_))

    val declaration = bindingDeclarations.getOrElseUpdate(name, new IndexBindingDeclaration(name, locations))
    declaration.replaceLocations(locations)
    fragmentDeclarationsCache.update(declaration)
    declaration
  }

  /**
    * Updates all locations of the binding declaration <b>in the relevant fragments</b>. Relevant fragments are
    * calculated from the given locations.
    */
  def updateBindingDeclarationByFragments(name: String, locations: Vector[Location]): IndexBindingDeclaration = {
    getBindingDeclaration(name) match {
      case Some(declaration) =>
        val fragmentUris = locations.map(_.getUri)

        declaration.removeLocationsByFragments(fragmentUris)
        fragmentUris.foreach(fragmentDeclarationsCache.remove(declaration, _))

        declaration.addLocations(locations)
        fragmentDeclarationsCache.update(declaration)

        declaration

      case None => replaceBindingDeclaration(name, locations)
    }
  }

  def removeBindingDeclaration(name: String): Unit = {
    bindingDeclarations.remove(name).foreach(fragmentDeclarationsCache.remove(_))
  }

}
