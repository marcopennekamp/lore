package lore.lsp.index

import lore.lsp.index.IndexTypeDeclaration.IndexMemberDeclaration
import org.eclipse.lsp4j.Location

import scala.collection.mutable

trait WithLocations {
  def locations: Vector[Location]
}

trait SingleLocation extends WithLocations {
  private var _location: Location = _

  def location: Location = _location
  override def locations: Vector[Location] = Vector(_location)

  def updateLocation(location: Location): Unit = {
    _location = location
  }
}

trait MultiLocation extends WithLocations {
  private var _locations: Vector[Location] = Vector.empty

  override def locations: Vector[Location] = _locations

  def addLocation(location: Location): Unit = {
    _locations = _locations :+ location
  }

  def addLocations(locations: Vector[Location]): Unit = locations.foreach(addLocation)

  def replaceLocations(locations: Vector[Location]): Unit = _locations = locations

  def removeLocationsByFragments(fragmentUris: Vector[String]): Unit = {
    _locations = _locations.filter(l => fragmentUris.contains(l.getUri))
  }
}

trait IndexDeclaration extends WithLocations {
  def name: String
}

class IndexTypeDeclaration(override val name: String, initialLocation: Location) extends IndexDeclaration with SingleLocation {
  updateLocation(initialLocation)

  private val _members: mutable.Map[String, IndexMemberDeclaration] = mutable.HashMap()

  def getMemberDeclaration(name: String): Option[IndexMemberDeclaration] = _members.get(name)

  def updateMemberDeclaration(name: String, location: Location): Unit = {
    _members.get(name) match {
      case Some(declaration) => declaration.updateLocation(location)
      case None => _members.update(name, new IndexMemberDeclaration(name, location))
    }
  }

  def removeMemberDeclarations(): Unit = _members.clear()
}

object IndexTypeDeclaration {
  class IndexMemberDeclaration(override val name: String, initialLocation: Location) extends IndexDeclaration with SingleLocation {
    updateLocation(initialLocation)
  }
}

class IndexBindingDeclaration(override val name: String, initialLocations: Vector[Location]) extends IndexDeclaration with MultiLocation {
  addLocations(initialLocations)
}
