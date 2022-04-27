package lore.compiler.poem.writer

import lore.compiler.core.CompilationException
import lore.compiler.poem.writer.ConstantsTable.{ConstantsIndex, IndexMap}
import lore.compiler.poem.{PoemFunctionInstance, PoemIntrinsic, PoemMetaShape, PoemType, PoemValue}
import lore.compiler.semantics.NamePath

import scala.collection.immutable.HashMap

/**
  * The constants table collects and indexes the constants required by all poem instructions of a poem fragment. This
  * process is carried out during writing.
  */
class ConstantsTable {
  import ConstantsTableEntry._

  private val index = new IndexMap[ConstantsTableEntry]

  def entries: Vector[ConstantsTableEntry] = index.entries

  def tpe(poemType: PoemType): ConstantsIndex = index.getOrInsert(TypeEntry(poemType))
  def value(poemValue: PoemValue): ConstantsIndex = index.getOrInsert(ValueEntry(poemValue))
  def name(name: String): ConstantsIndex = index.getOrInsert(NameEntry(name))
  def intrinsic(intrinsic: PoemIntrinsic): ConstantsIndex = index.getOrInsert(IntrinsicEntry(intrinsic))
  def schema(schema: NamePath): ConstantsIndex = index.getOrInsert(SchemaEntry(schema))
  def globalVariable(globalVariable: NamePath): ConstantsIndex = index.getOrInsert(GlobalVariableEntry(globalVariable))
  def multiFunction(multiFunction: NamePath): ConstantsIndex = index.getOrInsert(MultiFunctionEntry(multiFunction))
  def functionInstance(instance: PoemFunctionInstance): ConstantsIndex = index.getOrInsert(FunctionInstanceEntry(instance))
  def metaShape(metaShape: PoemMetaShape): ConstantsIndex = index.getOrInsert(MetaShapeEntry(metaShape))
}

sealed abstract class ConstantsTableEntry(val variant: ConstantsTableEntry.Variant.Value)

object ConstantsTableEntry {
  object Variant extends Enumeration {
    val Type, Val, Name, Intrinsic, Schema, GlobalVariable, MultiFunction, FunctionInstance, MetaShape = Value
  }

  case class TypeEntry(tpe: PoemType) extends ConstantsTableEntry(Variant.Type)
  case class ValueEntry(value: PoemValue) extends ConstantsTableEntry(Variant.Val)
  case class NameEntry(name: String) extends ConstantsTableEntry(Variant.Name)
  case class IntrinsicEntry(intrinsic: PoemIntrinsic) extends ConstantsTableEntry(Variant.Intrinsic)
  case class SchemaEntry(name: NamePath) extends ConstantsTableEntry(Variant.Schema)
  case class GlobalVariableEntry(name: NamePath) extends ConstantsTableEntry(Variant.GlobalVariable)
  case class MultiFunctionEntry(name: NamePath) extends ConstantsTableEntry(Variant.MultiFunction)
  case class FunctionInstanceEntry(instance: PoemFunctionInstance) extends ConstantsTableEntry(Variant.FunctionInstance)
  case class MetaShapeEntry(metaShape: PoemMetaShape) extends ConstantsTableEntry(Variant.MetaShape)
}

object ConstantsTable {
  type ConstantsIndex = Int

  /**
    * A constants table index is 16 bits wide, so the maximum index is 2**16 - 1.
    */
  val maximumIndex = 65535

  private class IndexMap[A] {
    private var _entries: Vector[A] = Vector.empty
    private var entryToIndex: Map[A, ConstantsIndex] = HashMap.empty

    def getOrInsert(entry: A): ConstantsIndex = synchronized {
      entryToIndex.get(entry) match {
        case Some(index) => index
        case None =>
          val index = _entries.length
          if (index > maximumIndex) {
            throw CompilationException(s"Cannot add entry $entry to the constants table: Maximum index of $maximumIndex exceeded.")
          }

          _entries = _entries :+ entry
          entryToIndex += entry -> index
          index
      }
    }

    def entries: Vector[A] = _entries
  }
}
