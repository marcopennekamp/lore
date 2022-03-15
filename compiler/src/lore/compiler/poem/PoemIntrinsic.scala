package lore.compiler.poem

/**
  * A PoemIntrinsic models the name and arity of the intrinsics known by the VM. Virtual intrinsics are known only to
  * the compiler and compiled to other poem instructions, such as `lore.list.get!` being compiled to `ListGet`.
  */
case class PoemIntrinsic(name: String, arity: Int, isVirtual: Boolean = false)

object PoemIntrinsic {

  private def unary(name: String, isVirtual: Boolean = false) = PoemIntrinsic(name, 1, isVirtual)
  private def binary(name: String, isVirtual: Boolean = false) = PoemIntrinsic(name, 2, isVirtual)

  // This should be kept in sync with `pyramid.nim`. (Except for virtual intrinsics.)
  val intrinsics = Vector(
    binary("lore.core.equal?"),
    binary("lore.core.less_than?"),
    unary("lore.core.to_string"),
    unary("lore.core.panic"),

    binary("lore.int.to_real"),

    unary("lore.string.length"),

    binary("lore.tuple.get!", isVirtual = true),

    binary("lore.list.get!", isVirtual = true),
    unary("lore.list.length", isVirtual = true),
    binary("lore.list.each"),

    unary("lore.io.println"),
  )

  val intrinsicsMap: Map[String, PoemIntrinsic] = intrinsics.map(intrinsic => (intrinsic.name, intrinsic)).toMap

}
