package lore.compiler.poem

import lore.compiler.core.CompilationException
import lore.compiler.semantics.functions.MultiFunctionDefinition
import lore.compiler.types.{BasicType, StructSchema}

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

/**
  * A function value always references an underlying multi-function.
  */
sealed trait PoemFunctionValue extends PoemValue {
  def mf: MultiFunctionDefinition
  def tpe: PoemType
}

case class PoemMultiFunctionValue(mf: MultiFunctionDefinition, tpe: PoemType) extends PoemFunctionValue

case class PoemFixedFunctionValue(mf: MultiFunctionDefinition, inputType: PoemType, tpe: PoemType) extends PoemFunctionValue

case class PoemLambdaFunctionValue(mf: MultiFunctionDefinition, tpe: PoemType) extends PoemFunctionValue {
  // The multi-function referenced by a lambda function value may only contain a SINGLE function. If not, the VM does
  // not accept the bytecode as valid.
  if (mf.functions.length != 1) {
    throw CompilationException(s"A poem lambda function value's multi-function must have exactly one function. Name: ${mf.name}.")
  }
}

case class PoemListValue(elements: Vector[PoemValue], tpe: PoemType) extends PoemValue

case class PoemShapeValue(properties: Map[String, PoemValue], tpe: PoemType) extends PoemValue with Poem.SortedProperties[PoemValue]

case class PoemSymbolValue(name: String) extends PoemValue {
  override lazy val tpe: PoemType = PoemSymbolType(name)
}

case class PoemStructValue(properties: Map[String, PoemValue], tpe: PoemType) extends PoemValue with Poem.SortedProperties[PoemValue]
