package lore.compiler.poem.writer

import lore.compiler.core.CompilationException

import java.io.ByteArrayOutputStream
import java.nio.{ByteBuffer, ByteOrder}
import java.nio.charset.Charset

/**
  * The bytecode writer writes various primitive data sizes in big endian.
  */
class BytecodeWriter {

  val output = new ByteArrayOutputStream()

  def writeUInt8(value: Int): Unit = {
    if (value > 255) {
      throw CompilationException(s"Cannot write value $value as a uint8: Maximum value exceeded.")
    }

    output.write((value & 0xFF).toByte)
  }

  def writeUInt16(value: Int): Unit = {
    if (value > 65535) {
      throw CompilationException(s"Cannot write value $value as a uint16: Maximum value exceeded.")
    }

    output.write(((value & 0xFF00) >> 8).toByte)
    output.write((value & 0xFF).toByte)
  }

  def writeInt16(value: Int): Unit = {
    if (value < Short.MinValue || value > Short.MaxValue) {
      throw CompilationException(s"Cannot write value $value as an int16: Maximum or minimum value exceeded.")
    }

    output.write(newBuffer(2).putShort(value.toShort).array())
  }

  def writeInt64(value: Long): Unit = output.writeBytes(newBuffer(8).putLong(value).array())

  def writeFloat64(value: Double): Unit = output.writeBytes(newBuffer(8).putDouble(value).array())

  def writeBoolean(value: Boolean): Unit = writeUInt8(if (value) 1 else 0)

  def writeString(string: String): Unit = {
    output.writeBytes(string.getBytes(Charset.forName("UTF-8")))
  }

  def writeStringWithLength(string: String): Unit = {
    val bytes = string.getBytes(Charset.forName("UTF-8"))
    writeUInt16(bytes.length)
    output.writeBytes(bytes)
  }

  private def newBuffer(capacity: Int): ByteBuffer = ByteBuffer.allocate(capacity).order(ByteOrder.BIG_ENDIAN)

}
