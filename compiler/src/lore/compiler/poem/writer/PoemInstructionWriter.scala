package lore.compiler.poem.writer

import lore.compiler.poem.PoemInstruction.InstanceKind
import lore.compiler.poem._
import lore.compiler.semantics.NamePath

object PoemInstructionWriter {

  def write(instruction: PoemInstruction)(implicit writer: BytecodeWriter, constantsTable: ConstantsTable): Unit = {
    writer.writeUInt16(instruction.operation.id)

    instruction match {
      case PoemInstruction.UnaryOperation(_, target, value) => write(target, value)
      case PoemInstruction.BinaryOperation(_, target, a, b) => write(target, a, b)

      case PoemInstruction.Assign(target, source) => write(target, source)

      case PoemInstruction.Const(target, value) =>
        write(target)
        writeConstantValue(value)

      case PoemInstruction.IntConst(target, value) =>
        write(target)
        writer.writeInt64(value)

      case PoemInstruction.IntToReal(target, value) => write(target, value)

      case PoemInstruction.BooleanConst(target, value) =>
        write(target)
        writer.writeBoolean16(value)

      case PoemInstruction.Tuple(target, elements) =>
        write(target)
        writeOperandsWithLength8(elements)

      case PoemInstruction.TupleGet(target, tuple, index) =>
        write(target, tuple)
        writer.writeUInt16(index)

      case PoemInstruction.FunctionCall(target, function, arguments) =>
        write(target, function)
        writeOperandsWithLength8(arguments)

      case PoemInstruction.FunctionSingle(target, mf, typeArguments) =>
        write(target)
        writeConstantMultiFunction(mf)
        writeOperandsWithLength8(typeArguments)

      case PoemInstruction.FunctionLambda(target, mf, tpe, capturedRegisters) =>
        write(target)
        writeConstantMultiFunction(mf)
        writeConstantType(tpe)
        writeOperandsWithLength16(capturedRegisters)

      case PoemInstruction.LambdaLocal(target, index) =>
        write(target)
        writer.writeUInt16(index)

      case PoemInstruction.List(target, tpe, elements) =>
        write(target)
        writeConstantType(tpe)
        writeOperandsWithLength16(elements)

      case PoemInstruction.ListAppend(_, target, list, element, tpe) =>
        write(target, list, element)
        writeConstantType(tpe)

      case PoemInstruction.ListAppendUntyped(target, list, element) => write(target, list, element)
      case PoemInstruction.ListLength(target, list) => write(target, list)
      case PoemInstruction.ListGet(target, list, index) => write(target, list, index)

      case PoemInstruction.Shape(target, metaShape, properties) =>
        write(target)
        writeConstantMetaShape(metaShape)
        writeOperandsWithLength8(properties)

      case PoemInstruction.Struct(target, tpe, valueArguments) =>
        write(target)
        writeConstantType(tpe)
        writeOperandsWithLength8(valueArguments)

      case PoemInstruction.StructPoly(target, schema, typeArguments, valueArguments) =>
        write(target)
        writeConstantSchema(schema.name)
        writeOperandsWithLength8(typeArguments)
        writeOperandsWithLength8(valueArguments)

      case PoemInstruction.StructEq(target, a, b) => write(target, a, b)

      case PoemInstruction.PropertyGet(target, instanceKind, instance, propertyName) =>
        write(target)
        write(instanceKind)
        write(instance)
        writeConstantName(propertyName)

      case PoemInstruction.PropertySet(instanceKind, instance, propertyName, value) =>
        write(instanceKind)
        write(instance)
        writeConstantName(propertyName)
        write(value)

      case PoemInstruction.Jump(target) => write(target)

      case PoemInstruction.JumpIfFalse(target, predicate) =>
        write(target)
        write(predicate)

      case PoemInstruction.JumpIfTrue(target, predicate) =>
        write(target)
        write(predicate)

      case PoemInstruction.Intrinsic(target, intrinsic, arguments) =>
        write(target)
        writeConstantIntrinsic(intrinsic)
        writeOperandsWithLength8(arguments)

      case PoemInstruction.GlobalGet(target, global) =>
        write(target)
        writeConstantGlobalVariable(global)

      case PoemInstruction.GlobalSet(global, value) =>
        writeConstantGlobalVariable(global)
        write(value)

      case PoemInstruction.Dispatch(target, mf, arguments) =>
        write(target)
        writeConstantMultiFunction(mf)
        writeOperandsWithLength8(arguments)

      case PoemInstruction.Call(target, functionInstance, valueArguments) =>
        write(target)
        writeConstantFunctionInstance(functionInstance)
        writeOperandsWithLength8(valueArguments)

      case PoemInstruction.CallPoly(target, mf, typeArguments, valueArguments) =>
        write(target)
        writeConstantMultiFunction(mf)
        writeOperandsWithLength8(typeArguments)
        writeOperandsWithLength8(valueArguments)

      case PoemInstruction.Return(value) => write(value)

      case PoemInstruction.TypeArg(target, index) =>
        write(target)
        writer.writeUInt16(index)

      case PoemInstruction.TypeConst(target, tpe) =>
        write(target)
        writeConstantType(tpe)

      case PoemInstruction.TypeOf(target, value) =>
        write(target)
        write(value)

      case PoemInstruction.TypePathIndex(target, tpe, index) =>
        write(target)
        write(tpe)
        writer.writeUInt16(index)

      case PoemInstruction.TypePathProperty(target, tpe, propertyName) =>
        write(target)
        write(tpe)
        writeConstantName(propertyName)

      case PoemInstruction.TypePathTypeArgument(target, tpe, schema, index) =>
        write(target)
        write(tpe)
        writeConstantSchema(schema.name)
        writer.writeUInt16(index)
    }
  }

  private def write(register: Poem.Register)(implicit writer: BytecodeWriter): Unit = writer.writeUInt16(register.id)

  private def write(registers: Poem.Register*)(implicit writer: BytecodeWriter): Unit = registers.foreach(write)

  private def writeOperandsWithLength8(operands: Vector[Poem.Register])(implicit writer: BytecodeWriter): Unit = {
    writer.writeUInt8(operands.length)
    write(operands: _*)
  }

  private def writeOperandsWithLength16(operands: Vector[Poem.Register])(implicit writer: BytecodeWriter): Unit = {
    writer.writeUInt16(operands.length)
    write(operands: _*)
  }

  private def write(location: Poem.Location)(implicit writer: BytecodeWriter): Unit = writer.writeUInt16(location.forcePc)

  private def writeConstantValue(value: PoemValue)(implicit writer: BytecodeWriter, constantsTable: ConstantsTable): Unit = {
    writer.writeUInt16(constantsTable.value(value))
  }

  private def writeConstantType(tpe: PoemType)(implicit writer: BytecodeWriter, constantsTable: ConstantsTable): Unit = {
    writer.writeUInt16(constantsTable.tpe(tpe))
  }

  private def writeConstantName(name: String)(implicit writer: BytecodeWriter, constantsTable: ConstantsTable): Unit = {
    writer.writeUInt16(constantsTable.name(name))
  }

  private def writeConstantIntrinsic(intrinsic: PoemIntrinsic)(implicit writer: BytecodeWriter, constantsTable: ConstantsTable): Unit = {
    writer.writeUInt16(constantsTable.intrinsic(intrinsic))
  }

  private def writeConstantSchema(name: NamePath)(implicit writer: BytecodeWriter, constantsTable: ConstantsTable): Unit = {
    writer.writeUInt16(constantsTable.schema(name))
  }

  private def writeConstantGlobalVariable(name: NamePath)(implicit writer: BytecodeWriter, constantsTable: ConstantsTable): Unit = {
    writer.writeUInt16(constantsTable.globalVariable(name))
  }

  private def writeConstantMultiFunction(name: NamePath)(implicit writer: BytecodeWriter, constantsTable: ConstantsTable): Unit = {
    writer.writeUInt16(constantsTable.multiFunction(name))
  }

  private def writeConstantFunctionInstance(instance: PoemFunctionInstance)(implicit writer: BytecodeWriter, constantsTable: ConstantsTable): Unit = {
    writer.writeUInt16(constantsTable.functionInstance(instance))
  }

  private def writeConstantMetaShape(metaShape: PoemMetaShape)(implicit writer: BytecodeWriter, constantsTable: ConstantsTable): Unit = {
    writer.writeUInt16(constantsTable.metaShape(metaShape))
  }

  private def write(instanceKind: InstanceKind)(implicit writer: BytecodeWriter, constantsTable: ConstantsTable): Unit = instanceKind match {
    case InstanceKind.Any => writer.writeUInt8(0)
    case InstanceKind.Shape => writer.writeUInt8(1)
    case InstanceKind.Trait => writer.writeUInt8(2)
    case InstanceKind.Struct(instanceSchema) =>
      writer.writeUInt8(3)
      writeConstantSchema(instanceSchema.name)
  }

}
