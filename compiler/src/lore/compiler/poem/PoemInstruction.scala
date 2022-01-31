package lore.compiler.poem
import lore.compiler.poem.Poem.PoemRegister
import lore.compiler.poem.PoemOperation.PoemOperation
import lore.compiler.semantics.NamePath
import lore.compiler.semantics.functions.MultiFunctionDefinition
import lore.compiler.semantics.variables.GlobalVariableDefinition
import lore.compiler.types.DeclaredSchema

/**
  * A PoemInstruction is a representation of all possible VM instructions.
  *
  * The biggest difference to the Poem bytecode format is that the constant table will be assembled by the Poem writer.
  * Hence, a PoemInstruction doesn't contain index references to the constant table entries, but the constants
  * themselves.
  *
  * PoemInstructions are classes, not case classes, because their labels are guaranteed to be unique.
  */
class PoemInstruction(val operation: PoemOperation, val operands: Vector[PoemInstruction.Operand]) {
  lazy val label: PoemLabel = new PoemLabel()
}

/**
  * PoemLabels are referentially equal objects which are used to resolve program counter values of jump instructions.
  */
class PoemLabel {
  override def equals(obj: Any): Boolean = obj match {
    case other: PoemLabel => this eq other
    case _ => false
  }
}

object PoemInstruction {
  sealed trait Operand
  object Operand {
    case class Reg(id: PoemRegister) extends Operand

    /**
      * A uint16 operand interpreted from the argument bits.
      */
    case class Usg(value: Int) extends Operand

    /**
      * An int16 operand interpreted from the argument bits.
      */
    case class Sig(value: Int) extends Operand

    case class Val(value: PoemValue) extends Operand
    case class Tpe(tpe: PoemType) extends Operand
    case class Nam(name: String) extends Operand
    case class Intr(name: String) extends Operand
    case class Sch(name: NamePath) extends Operand
    case class Glb(name: NamePath) extends Operand
    case class Mf(name: NamePath) extends Operand
    case class Mtsh(metaShape: PoemMetaShape) extends Operand
    case class Targ(id: Int) extends Operand

    /**
      * A target label of a jump instruction. The actual target instruction index will be resolved once instructions
      * are assembled in a flat list.
      */
    case class Lbl(label: PoemLabel) extends Operand
  }

  def apply(operation: PoemOperation, operands: Operand*): PoemInstruction = new PoemInstruction(operation, operands.toVector)

  // Argument types for instruction creation that haven't been converted to Operands.
  type AReg = PoemRegister
  type AVal = PoemValue
  type ATpe = PoemType
  type ASch = DeclaredSchema
  type AGlb = GlobalVariableDefinition
  type AMf = MultiFunctionDefinition
  type AMtsh = PoemMetaShape

  private def inst(operation: PoemOperation) =
    PoemInstruction(operation)

  private def instReg(operation: PoemOperation, a1: AReg) =
    PoemInstruction(operation, Operand.Reg(a1))

  private def instIntr(operation: PoemOperation, intrinsic: String) =
    PoemInstruction(operation, Operand.Intr(intrinsic))

  private def instLbl(operation: PoemOperation, label: PoemLabel) =
    PoemInstruction(operation, Operand.Lbl(label))

  private def instIntrReg(operation: PoemOperation, intrinsic: String, a1: AReg) =
    PoemInstruction(operation, Operand.Intr(intrinsic), Operand.Reg(a1))

  private def instGlbReg(operation: PoemOperation, globalVariable: AGlb, a1: AReg) =
    PoemInstruction(operation, Operand.Glb(globalVariable.name), Operand.Reg(a1))

  private def instLblReg(operation: PoemOperation, label: PoemLabel, a1: AReg) =
    PoemInstruction(operation, Operand.Lbl(label), Operand.Reg(a1))

  private def instIntrReg2(operation: PoemOperation, intrinsic: String, a1: AReg, a2: AReg) =
    PoemInstruction(operation, Operand.Intr(intrinsic), Operand.Reg(a1), Operand.Reg(a2))

  private def opReg(operation: PoemOperation, target: AReg, value: AReg) =
    PoemInstruction(operation, Operand.Reg(target), Operand.Reg(value))

  private def opSig(operation: PoemOperation, target: AReg, value: Int) =
    PoemInstruction(operation, Operand.Reg(target), Operand.Sig(value))

  private def opVal(operation: PoemOperation, target: AReg, value: AVal) =
    PoemInstruction(operation, Operand.Reg(target), Operand.Val(value))

  private def opTpe(operation: PoemOperation, target: AReg, tpe: ATpe) =
    PoemInstruction(operation, Operand.Reg(target), Operand.Tpe(tpe))

  private def opIntr(operation: PoemOperation, target: AReg, intrinsic: String) =
    PoemInstruction(operation, Operand.Reg(target), Operand.Intr(intrinsic))

  private def opGlb(operation: PoemOperation, target: AReg, globalVariable: AGlb) =
    PoemInstruction(operation, Operand.Reg(target), Operand.Glb(globalVariable.name))

  private def opTarg(operation: PoemOperation, target: AReg, index: Int) =
    PoemInstruction(operation, Operand.Reg(target), Operand.Targ(index))

  private def opReg2(operation: PoemOperation, target: AReg, a1: AReg, a2: AReg) =
    PoemInstruction(operation, Operand.Reg(target), Operand.Reg(a1), Operand.Reg(a2))

  private def opRegUsg(operation: PoemOperation, target: AReg, a1: AReg, a2: Int) =
    PoemInstruction(operation, Operand.Reg(target), Operand.Reg(a1), Operand.Usg(a2))

  private def opRegSig(operation: PoemOperation, target: AReg, a1: AReg, a2: Int) =
    PoemInstruction(operation, Operand.Reg(target), Operand.Reg(a1), Operand.Sig(a2))

  private def opRegVal(operation: PoemOperation, target: AReg, a1: AReg, a2: AVal) =
    PoemInstruction(operation, Operand.Reg(target), Operand.Reg(a1), Operand.Val(a2))

  private def opRegNam(operation: PoemOperation, target: AReg, a1: AReg, name: String) =
    PoemInstruction(operation, Operand.Reg(target), Operand.Reg(a1), Operand.Nam(name))

  private def opValReg(operation: PoemOperation, target: AReg, a1: AVal, a2: AReg) =
    PoemInstruction(operation, Operand.Reg(target), Operand.Val(a1), Operand.Reg(a2))

  private def opIntrReg(operation: PoemOperation, target: AReg, intrinsic: String, a1: AReg) =
    PoemInstruction(operation, Operand.Reg(target), Operand.Intr(intrinsic), Operand.Reg(a1))

  private def opSchReg(operation: PoemOperation, target: AReg, schema: ASch, a1: AReg) =
    PoemInstruction(operation, Operand.Reg(target), Operand.Sch(schema.name), Operand.Reg(a1))

  private def opMfReg(operation: PoemOperation, target: AReg, mf: AMf, a1: AReg) =
    PoemInstruction(operation, Operand.Reg(target), Operand.Mf(mf.name), Operand.Reg(a1))

  private def opMtshReg(operation: PoemOperation, target: AReg, mtsh: AMtsh, a1: AReg) =
    PoemInstruction(operation, Operand.Reg(target), Operand.Mtsh(mtsh), Operand.Reg(a1))

  private def opReg3(operation: PoemOperation, target: AReg, a1: AReg, a2: AReg, a3: AReg) =
    PoemInstruction(operation, Operand.Reg(target), Operand.Reg(a1), Operand.Reg(a2), Operand.Reg(a3))

  private def opReg2Tpe(operation: PoemOperation, target: AReg, a1: AReg, a2: AReg, a3: ATpe) =
    PoemInstruction(operation, Operand.Reg(target), Operand.Reg(a1), Operand.Reg(a2), Operand.Tpe(a3))

  private def opIntrReg2(operation: PoemOperation, target: AReg, intrinsic: String, a1: AReg, a2: AReg) =
    PoemInstruction(operation, Operand.Reg(target), Operand.Intr(intrinsic), Operand.Reg(a1), Operand.Reg(a2))

  private def opSchReg2(operation: PoemOperation, target: AReg, schema: ASch, a1: AReg, a2: AReg) =
    PoemInstruction(operation, Operand.Reg(target), Operand.Sch(schema.name), Operand.Reg(a1), Operand.Reg(a2))

  private def opMfReg2(operation: PoemOperation, target: AReg, mf: AMf, a1: AReg, a2: AReg) =
    PoemInstruction(operation, Operand.Reg(target), Operand.Mf(mf.name), Operand.Reg(a1), Operand.Reg(a2))

  private def opMtshReg2(operation: PoemOperation, target: AReg, mtsh: AMtsh, a1: AReg, a2: AReg) =
    PoemInstruction(operation, Operand.Reg(target), Operand.Mtsh(mtsh), Operand.Reg(a1), Operand.Reg(a2))

  private def opSchReg4(operation: PoemOperation, target: AReg, schema: ASch, a1: AReg, a2: AReg, a3: AReg, a4: AReg) =
    PoemInstruction(operation, Operand.Reg(target), Operand.Sch(schema.name), Operand.Reg(a1), Operand.Reg(a2), Operand.Reg(a3), Operand.Reg(a4))

  def const(target: AReg, value: AVal): PoemInstruction = opVal(PoemOperation.Const, target, value)
  def constPoly(target: AReg, value: AVal): PoemInstruction = opVal(PoemOperation.ConstPoly, target, value)

  def intConst(target: AReg, value: Int): PoemInstruction = opSig(PoemOperation.IntConst, target, value)
  def intAdd(target: AReg, a: AReg, b: AReg): PoemInstruction = opReg2(PoemOperation.IntAdd, target, a, b)
  def intAddConst(target: AReg, a: AReg, b: Int): PoemInstruction = opRegSig(PoemOperation.IntAddConst, target, a, b)
  def intSubConst(target: AReg, a: AReg, b: Int): PoemInstruction = opRegSig(PoemOperation.IntSubConst, target, a, b)
  def intLt(target: AReg, a: AReg, b: AReg): PoemInstruction = opReg2(PoemOperation.IntAdd, target, a, b)
  def intLtConst(target: AReg, a: AReg, b: Int): PoemInstruction = opRegSig(PoemOperation.IntLtConst, target, a, b)
  def intGtConst(target: AReg, a: AReg, b: Int): PoemInstruction = opRegSig(PoemOperation.IntGtConst, target, a, b)

  def realAdd(target: AReg, a: AReg, b: AReg): PoemInstruction = opReg2(PoemOperation.IntAdd, target, a, b)

  def stringOf(target: AReg, value: AReg): PoemInstruction = opReg(PoemOperation.StringOf, target, value)
  def stringConcat(target: AReg, a: AReg, b: AReg): PoemInstruction = opReg2(PoemOperation.IntAdd, target, a, b)
  def stringConcatConst(target: AReg, a: AReg, b: AVal): PoemInstruction = opRegVal(PoemOperation.StringConcatConst, target, a, b)
  def stringConcatConstl(target: AReg, a: AVal, b: AReg): PoemInstruction = opValReg(PoemOperation.StringConcatConst, target, a, b)

  def tuple(target: AReg, first: AReg, last: AReg): PoemInstruction = opReg2(PoemOperation.Tuple, target, first, last)
  def tuple2(target: AReg, a: AReg, b: AReg): PoemInstruction = opReg2(PoemOperation.Tuple2, target, a, b)
  def tupleGet(target: AReg, tuple: AReg, index: Int): PoemInstruction = opRegUsg(PoemOperation.TupleGet, target, tuple, index)

  def functionCall0(target: AReg, function: AReg): PoemInstruction = opReg(PoemOperation.FunctionCall0, target, function)
  def functionCall1(target: AReg, function: AReg, argument: AReg): PoemInstruction = opReg2(PoemOperation.FunctionCall1, target, function, argument)
  def functionCall2(target: AReg, function: AReg, argument1: AReg, argument2: AReg): PoemInstruction = opReg3(PoemOperation.FunctionCall2, target, function, argument1, argument2)

  // TODO (bytecode): Can't we decide whether to use Poly right here? If `tpe` contains a type variable, it must be Poly, no?
  def listAppend(target: AReg, list: AReg, element: AReg, tpe: ATpe): PoemInstruction = opReg2Tpe(PoemOperation.ListAppend, target, list, element, tpe)
  def listAppendPoly(target: AReg, list: AReg, element: AReg, tpe: ATpe): PoemInstruction = opReg2Tpe(PoemOperation.ListAppendPoly, target, list, element, tpe)
  def listAppendUntyped(target: AReg, list: AReg, element: AReg): PoemInstruction = opReg2(PoemOperation.ListAppendUntyped, target, list, element)

  def shape(target: AReg, metaShape: AMtsh, first: AReg, last: AReg): PoemInstruction = opMtshReg2(PoemOperation.Shape, target, metaShape, first, last)
  def shape1(target: AReg, metaShape: AMtsh, a1: AReg): PoemInstruction = opMtshReg(PoemOperation.Shape, target, metaShape, a1)
  def shape2(target: AReg, metaShape: AMtsh, a1: AReg, a2: AReg): PoemInstruction = opMtshReg2(PoemOperation.Shape, target, metaShape, a1, a2)
  def shapeGetProperty(target: AReg, shape: AReg, propertyName: String): PoemInstruction = opRegNam(PoemOperation.ShapeGetProperty, target, shape, propertyName)

  def symbolEq(target: AReg, a: AReg, b: AReg): PoemInstruction = opReg2(PoemOperation.SymbolEq, target, a, b)
  def symbolEqConst(target: AReg, a: AReg, b: AVal): PoemInstruction = opRegVal(PoemOperation.SymbolEqConst, target, a, b)

  def struct(target: AReg, schema: ASch, first: AReg, last: AReg): PoemInstruction = opSchReg2(PoemOperation.Struct, target, schema, first, last)
  def struct1(target: AReg, schema: ASch, a1: AReg): PoemInstruction = opSchReg(PoemOperation.Struct1, target, schema, a1)
  def struct2(target: AReg, schema: ASch, a1: AReg, a2: AReg): PoemInstruction = opSchReg2(PoemOperation.Struct2, target, schema, a1, a2)
  def structPoly(target: AReg, schema: ASch, firstTarg: AReg, lastTarg: AReg, firstArg: AReg, lastArg: AReg): PoemInstruction = opSchReg4(PoemOperation.StructPoly, target, schema, firstTarg, lastTarg, firstArg, lastArg)
  def structGetProperty(target: AReg, struct: AReg, index: Int): PoemInstruction = opRegUsg(PoemOperation.StructGetProperty, target, struct, index)
  def structGetNamedProperty(target: AReg, struct: AReg, propertyName: String): PoemInstruction = opRegNam(PoemOperation.StructGetNamedProperty, target, struct, propertyName)
  def structEq(target: AReg, a1: AReg, a2: AReg): PoemInstruction = opReg2(PoemOperation.StructEq, target, a1, a2)

  def jump(target: PoemLabel): PoemInstruction = instLbl(PoemOperation.Jump, target)
  def jumpIfFalse(target: PoemLabel, predicate: AReg): PoemInstruction = instLblReg(PoemOperation.JumpIfFalse, target, predicate)
  def jumpIfTrue(target: PoemLabel, predicate: AReg): PoemInstruction = instLblReg(PoemOperation.JumpIfTrue, target, predicate)

  def intrinsic0(target: AReg, intrinsic: String): PoemInstruction = opIntr(PoemOperation.Intrinsic0, target, intrinsic)
  def intrinsic1(target: AReg, intrinsic: String, a1: AReg): PoemInstruction = opIntrReg(PoemOperation.Intrinsic1, target, intrinsic, a1)
  def intrinsic2(target: AReg, intrinsic: String, a1: AReg, a2: AReg): PoemInstruction = opIntrReg2(PoemOperation.Intrinsic2, target, intrinsic, a1, a2)
  def intrinsicVoid0(intrinsic: String): PoemInstruction = instIntr(PoemOperation.IntrinsicVoid0, intrinsic)
  def intrinsicVoid1(intrinsic: String, a1: AReg): PoemInstruction = instIntrReg(PoemOperation.IntrinsicVoid1, intrinsic, a1)
  def intrinsicFa1(target: AReg, intrinsic: String, a1: AReg): PoemInstruction = opIntrReg(PoemOperation.IntrinsicFa1, target, intrinsic, a1)
  def intrinsicFa2(target: AReg, intrinsic: String, a1: AReg, a2: AReg): PoemInstruction = opIntrReg2(PoemOperation.IntrinsicFa2, target, intrinsic, a1, a2)
  def intrinsicVoidFa2(intrinsic: String, a1: AReg, a2: AReg): PoemInstruction = instIntrReg2(PoemOperation.IntrinsicVoidFa2, intrinsic, a1, a2)

  def globalGetEager(target: AReg, globalVariable: AGlb): PoemInstruction = opGlb(PoemOperation.GlobalGetEager, target, globalVariable)
  def globalGetLazy(target: AReg, globalVariable: AGlb): PoemInstruction = opGlb(PoemOperation.GlobalGetLazy, target, globalVariable)
  def globalSet(globalVariable: AGlb, value: AReg): PoemInstruction = instGlbReg(PoemOperation.GlobalSet, globalVariable, value)

  def dispatch1(target: AReg, mf: AMf, a1: AReg): PoemInstruction = opMfReg(PoemOperation.Dispatch1, target, mf, a1)
  def dispatch2(target: AReg, mf: AMf, a1: AReg, a2: AReg): PoemInstruction = opMfReg2(PoemOperation.Dispatch1, target, mf, a1, a2)

  def `return`(value: AReg): PoemInstruction = instReg(PoemOperation.Return, value)
  def returnUnit(): PoemInstruction = inst(PoemOperation.ReturnUnit)
  def return0(): PoemInstruction = inst(PoemOperation.Return0)

  def typeArg(target: AReg, index: Int): PoemInstruction = opTarg(PoemOperation.TypeArg, target, index)
  def typeConst(target: AReg, tpe: ATpe): PoemInstruction = opTpe(PoemOperation.TypeConst, target, tpe)
}
