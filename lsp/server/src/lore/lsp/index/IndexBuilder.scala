package lore.lsp.index

import lore.compiler.core.Position
import lore.compiler.semantics.Registry
import lore.compiler.semantics.functions.MultiFunctionDefinition
import lore.compiler.semantics.structures.{StructDefinition, SchemaDefinition}
import lore.lsp.utils.PositionUtil

object IndexBuilder {

  def updateTypeDeclaration(name: String, properties: Vector[(String, Position)], position: Position)(implicit globalIndex: GlobalIndex): IndexTypeDeclaration = {
    val location = PositionUtil.toLocation(position)
    val declaration = globalIndex.updateTypeDeclaration(name, location)

    declaration.removeMemberDeclarations()
    properties.foreach {
      case (name, position) =>
        val propertyLocation = PositionUtil.toLocation(position)
        declaration.updateMemberDeclaration(name, propertyLocation)
    }

    declaration
  }

  def updateBindingDeclarationByFragments(name: String, positions: Vector[Position])(implicit globalIndex: GlobalIndex): IndexBindingDeclaration = {
    val locations = positions.map(PositionUtil.toLocation)
    globalIndex.updateBindingDeclarationByFragments(name, locations)
  }

  /**
    * Builds a fresh global index from the given registry.
    */
  def fromRegistry(registry: Registry): GlobalIndex = {
    implicit val globalIndex: GlobalIndex = new GlobalIndex

    // We have to filter out type definitions that are internally added by the compiler!
    registry.schemaDefinitions.values.filter(_.position != Position.internal).foreach(addSchemaDefinition)
    registry.multiFunctions.values.foreach(addMultiFunctionDefinition)

    globalIndex
  }

  def addSchemaDefinition(definition: SchemaDefinition)(implicit globalIndex: GlobalIndex): Unit = {
    val properties = definition match {
      case struct: StructDefinition => struct.properties.map(p => (p.name, p.position))
      case _ => Vector.empty
    }
    updateTypeDeclaration(definition.name, properties, definition.position)
  }

  def addMultiFunctionDefinition(mf: MultiFunctionDefinition)(implicit globalIndex: GlobalIndex): Unit = {
    val locations = mf.functions.map(f => PositionUtil.toLocation(f.position))
    globalIndex.replaceBindingDeclaration(mf.name, locations)
  }

}
