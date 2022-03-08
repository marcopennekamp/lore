package lore.compiler.poem

import lore.compiler.poem.PoemFunctionValueVariant.PoemFunctionValueVariant
import lore.compiler.semantics.NamePath
import lore.compiler.types.BasicType

sealed trait PoemValue {
  def tpe: PoemType
}

case class PoemIntValue(value: Long) extends PoemValue {
  override def tpe: PoemType = PoemBasicType(BasicType.Int)
}

case class PoemRealValue(value: Double) extends PoemValue {
  override def tpe: PoemType = PoemBasicType(BasicType.Real)
}

case class PoemBooleanValue(value: Boolean) extends PoemValue {
  override def tpe: PoemType = PoemBasicType(BasicType.Boolean)
}

case class PoemStringValue(value: String) extends PoemValue {
  override def tpe: PoemType = PoemBasicType(BasicType.String)
}

case class PoemTupleValue(elements: Vector[PoemValue], tpe: PoemType) extends PoemValue

object PoemFunctionValueVariant extends Enumeration {
  type PoemFunctionValueVariant = Value
  val Multi, Single, Fixed = Value
}

/**
  * A function value always references an underlying multi-function by name. They cannot be referenced as
  * MultiFunctionDefinitions because the assembly phase may generate additional multi-functions which haven't been
  * resolved as MultiFunctionDefinitions.
  */
sealed trait PoemFunctionValue extends PoemValue {
  def variant: PoemFunctionValueVariant
  def mf: NamePath
  def tpe: PoemType
}

case class PoemMultiFunctionValue(mf: NamePath, tpe: PoemType) extends PoemFunctionValue {
  override val variant: PoemFunctionValueVariant = PoemFunctionValueVariant.Multi
}

case class PoemSingleFunctionValue(mf: NamePath, typeArguments: Vector[PoemType], tpe: PoemType) extends PoemFunctionValue {
  override val variant: PoemFunctionValueVariant = PoemFunctionValueVariant.Single
}

case class PoemFixedFunctionValue(mf: NamePath, inputType: PoemType, tpe: PoemType) extends PoemFunctionValue {
  override val variant: PoemFunctionValueVariant = PoemFunctionValueVariant.Fixed
}

case class PoemListValue(elements: Vector[PoemValue], tpe: PoemType) extends PoemValue

case class PoemShapeValue(properties: Map[String, PoemValue], tpe: PoemType) extends PoemValue with Poem.SortedProperties[PoemValue]

case class PoemSymbolValue(name: String) extends PoemValue {
  override lazy val tpe: PoemType = PoemSymbolType(name)
}

case class PoemStructValue(properties: Map[String, PoemValue], tpe: PoemType) extends PoemValue with Poem.SortedProperties[PoemValue]
