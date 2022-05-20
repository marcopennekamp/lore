package lore.compiler.poem

/**
  * A PoemIntrinsic models the name and arity of the intrinsics known by the VM. Virtual intrinsics are known only to
  * the compiler and compiled to other poem instructions, such as `lore.list.get` being compiled to `ListGet`.
  */
case class PoemIntrinsic(name: String, arity: Int, isVirtual: Boolean = false)

object PoemIntrinsic {

  private def intr(name: String, arity: Int, isVirtual: Boolean = false) = PoemIntrinsic(name, arity)

  // This should be kept in sync with `pyramid.nim`. (Except for virtual intrinsics.)
  val intrinsics: Vector[PoemIntrinsic] = Vector(
    intr("lore.core.equal?", 3),
    intr("lore.core.less_than?", 3),
    intr("lore.core.to_string", 2),
    intr("lore.core.type_of", 1),
    intr("lore.core.subtype?", 2),
    intr("lore.core.panic", 1),

    intr("lore.int.remainder", 2),
    intr("lore.int.to_real", 1),

    intr("lore.real.nan?", 1),
    intr("lore.real.to_int", 1),
    intr("lore.real.floor", 1),
    intr("lore.real.ceil", 1),
    intr("lore.real.round", 1),
    intr("lore.real.pow", 2),
    intr("lore.real.parse", 1),

    intr("lore.string.length", 1),
    intr("lore.string.at", 2),
    intr("lore.string.at_index", 2),
    intr("lore.string.byte_size", 1),
    intr("lore.string.byte_at", 2),
    intr("lore.string.to_lower", 1),
    intr("lore.string.to_upper", 1),

    intr("lore.symbol.name", 1),

    intr("lore.tuple.get", 2, isVirtual = true),

    intr("lore.list.get", 2, isVirtual = true),
    intr("lore.list.length", 1, isVirtual = true),
    intr("lore.list.concat", 2),
    intr("lore.list.slice", 3),
    intr("lore.list.flatten", 1),
    intr("lore.list.map", 2),
    intr("lore.list.flat_map", 2),
    intr("lore.list.each", 2),
    intr("lore.list.filter", 2),

    intr("lore.io.println", 1),

    intr("lore.test.raise_assertion_error", 1),
  )

  val intrinsicsMap: Map[String, PoemIntrinsic] = intrinsics.map(intrinsic => (intrinsic.name, intrinsic)).toMap

}
