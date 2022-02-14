package lore.compiler.poem

case class PoemIntrinsic(name: String, arity: Int)

object PoemIntrinsic {
  // This should be kept in sync with `pyramid.nim`.
  val intrinsics = Vector(
    PoemIntrinsic("lore.core.panic", 0),
    PoemIntrinsic("lore.strings.length", 1),
    PoemIntrinsic("lore.lists.each", 2),
    PoemIntrinsic("lore.io.println", 1),
  )

  val intrinsicsMap: Map[String, PoemIntrinsic] = intrinsics.map(intrinsic => (intrinsic.name, intrinsic)).toMap
}
