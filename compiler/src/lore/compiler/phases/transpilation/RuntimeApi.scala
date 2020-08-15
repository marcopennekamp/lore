package lore.compiler.phases.transpilation

object RuntimeApi {
  private val base = "Lore"

  object types {
    val base = s"${RuntimeApi.base}.types"

    // Type constants.
    val any = s"$base.any"
    val nothing = s"$base.nothing"
    val real = s"$base.real"
    val int = s"$base.int"
    val boolean = s"$base.boolean"
    val string = s"$base.string"
    val unit = s"$base.unit"

    // Type constructors.
    val variable = s"$base.variable"
    val intersection = s"$base.intersection"
    val sum = s"$base.sum"
    val product = s"$base.product"
    val component = s"$base.component"
    val list = s"$base.list"
    val map = s"$base.map"
    val classType = s"$base.classType"

    // Unsafe constructors.
    object unsafe {
      val base = s"${RuntimeApi.types.base}.unsafe"

      val unhashedProduct = s"$base.unhashedProduct"
    }

    // Type relationships.
    val isSubtype = s"$base.isSubtype"
    val areEqual = s"$base.areEqual"
    val fits = s"$base.fits"
    val fitsMonomorphic = s"$base.fitsMonomorphic"
    val fitsPolymorphic = s"$base.fitsPolymorphic"
    val typeOf = s"$base.typeOf"
  }

  object values {
    val base = s"${RuntimeApi.base}.values"

    // Value APIs.
    object list {
      val base = s"${RuntimeApi.values.base}.list"

      val create = s"$base.create"
      val append = s"$base.append"
      val forEach = s"$base.forEach"
    }

    object tuple {
      val base = s"${RuntimeApi.values.base}.tuple"

      val create = s"$base.create"
      val unit = s"$base.unit"
    }

    object map {
      val base = s"${RuntimeApi.values.base}.map"

      val create = s"$base.create"
      val entries = s"$base.entries"
    }

    // Core functions operating on values of any type.
    val areEqual = s"$base.areEqual"
    val isLessThan = s"$base.isLessThan"
    val hash = s"$base.hash"
    val loreToString = s"$base.toString" // TODO: This clashes with the JVM toString. Maybe we should rename it anyway?
  }

  object utils {
    val base = s"${RuntimeApi.base}.utils"

    object tinyMap {
      val base = s"${RuntimeApi.utils.base}.tinyMap"

      val get = s"$base.get"
      val add = s"$base.add"
    }

    object tinySet {
      val base = s"${RuntimeApi.utils.base}.tinySet"

      val has = s"$base.has"
      val add = s"$base.add"
    }

    object hashMap {
      val base = s"${RuntimeApi.utils.base}.hashMap"

      val create = s"$base.create"
    }

    object typeMap {
      val base = s"${RuntimeApi.utils.base}.typeMap"

      val create = s"$base.create"
    }

    object error {
      val base = s"${RuntimeApi.utils.base}.error"

      val ambiguousCall = s"$base.ambiguousCall"
      val emptyFit = s"$base.emptyFit"
      val missingImplementation = s"$base.missingImplementation"
    }
  }
}