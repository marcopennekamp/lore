package lore.compiler.build

import org.json4s._
import org.json4s.native.JsonMethods

import java.nio.file.Path

/**
  * Parses Lore build options from a JSON string. An example file with default values is located at `lore.json`.
  */
object JsonBuildOptionsParser {

  private val pathSerializer: Serializer[Path] = new Serializer[Path] {
    override def deserialize(implicit format: Formats): PartialFunction[(TypeInfo, JValue), Path] = {
      case (_, JString(value)) => Path.of(value)
    }
    override def serialize(implicit format: Formats): PartialFunction[Any, JValue] = throw new UnsupportedOperationException
  }

  private implicit val formats: Formats = DefaultFormats + pathSerializer

  def parse(json: String): BuildOptions = JsonMethods.parse(json).extract[BuildOptions]

}
