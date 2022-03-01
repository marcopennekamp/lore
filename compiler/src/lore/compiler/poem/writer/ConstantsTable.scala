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

  private val typesIndex = new IndexMap[PoemType]
  private val valuesIndex = new IndexMap[PoemValue]
  private val namesIndex = new IndexMap[String]
  private val intrinsicsIndex = new IndexMap[PoemIntrinsic]
  private val schemasIndex = new IndexMap[NamePath]
  private val globalVariablesIndex = new IndexMap[NamePath]
  private val multiFunctionsIndex = new IndexMap[NamePath]
  private val functionInstancesIndex = new IndexMap[PoemFunctionInstance]
  private val metaShapesIndex = new IndexMap[PoemMetaShape]

  def tpe(poemType: PoemType): ConstantsIndex = typesIndex.getOrInsert(poemType)
  def value(poemValue: PoemValue): ConstantsIndex = valuesIndex.getOrInsert(poemValue)
  def name(name: String): ConstantsIndex = namesIndex.getOrInsert(name)
  def intrinsic(intrinsic: PoemIntrinsic): ConstantsIndex = intrinsicsIndex.getOrInsert(intrinsic)
  def schema(schema: NamePath): ConstantsIndex = schemasIndex.getOrInsert(schema)
  def globalVariable(globalVariable: NamePath): ConstantsIndex = globalVariablesIndex.getOrInsert(globalVariable)
  def multiFunction(multiFunction: NamePath): ConstantsIndex = multiFunctionsIndex.getOrInsert(multiFunction)
  def functionInstance(instance: PoemFunctionInstance): ConstantsIndex = functionInstancesIndex.getOrInsert(instance)
  def metaShape(metaShape: PoemMetaShape): ConstantsIndex = metaShapesIndex.getOrInsert(metaShape)

  def types: Vector[PoemType] = typesIndex.entries
  def values: Vector[PoemValue] = valuesIndex.entries
  def names: Vector[String] = namesIndex.entries
  def intrinsics: Vector[PoemIntrinsic] = intrinsicsIndex.entries
  def schemas: Vector[NamePath] = schemasIndex.entries
  def globalVariables: Vector[NamePath] = globalVariablesIndex.entries
  def multiFunctions: Vector[NamePath] = multiFunctionsIndex.entries
  def functionInstances: Vector[PoemFunctionInstance] = functionInstancesIndex.entries
  def metaShapes: Vector[PoemMetaShape] = metaShapesIndex.entries

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
