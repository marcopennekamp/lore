package lore.compiler.phases.transpilation

// TODO: Return TranspiledName values instead of plain strings?

object RuntimeApi {
  private val base = "Lore"

  object types {
    val base = s"${RuntimeApi.base}.types"

    val any = s"$base.any"
    val nothing = s"$base.nothing"
    val real = s"$base.real"
    val int = s"$base.int"
    val boolean = s"$base.boolean"
    val string = s"$base.string"

    val variable = s"$base.variable"

    val isSubtype = s"$base.isSubtype"
    val areEqual = s"$base.areEqual"
    val fits = s"$base.fits"
    val fitsMonomorphic = s"$base.fitsMonomorphic"
    val fitsPolymorphic = s"$base.fitsPolymorphic"
    val typeOf = s"$base.typeOf"
    val isPolymorphic = s"$base.isPolymorphic"
    val variables = s"$base.variables"

    object introspection {
      val base = s"${RuntimeApi.types.base}.introspection"

      val initialize = s"${base}.initialize"
    }
  }

  object values {
    val base = s"${RuntimeApi.base}.values"

    val areEqual = s"$base.areEqual"
    val isLessThan = s"$base.isLessThan"
    val hash = s"$base.hash"
    val loreToString = s"$base.toString" // TODO: This clashes with the JVM toString. Maybe we should rename it anyway?
  }

  object sums {
    val base = s"${RuntimeApi.base}.sums"

    val tpe = s"$base.type"
    val simplified = s"$base.simplified"
  }

  object intersections {
    val base = s"${RuntimeApi.base}.intersections"

    val tpe = s"$base.type"
    val simplified = s"$base.simplified"
  }

  object tuples {
    val base = s"${RuntimeApi.base}.tuples"

    val tpe = s"$base.type"
    val unhashedType = s"$base.unhashedType"
    val unitType = s"$base.unitType"
    val value = s"$base.value"
    val unitValue = s"$base.unitValue"
    val get = s"$base.get"
  }

  object lists {
    val base = s"${RuntimeApi.base}.lists"

    val tpe = s"$base.type"
    val value = s"$base.value"
    val append = s"$base.append"
    val get = s"$base.get"
    val forEach = s"$base.forEach"
    val length = s"$base.lenght"
  }

  object maps {
    val base = s"${RuntimeApi.base}.maps"

    val tpe = s"$base.type"
    val value = s"$base.value"
    val set = s"$base.set"
    val get = s"$base.get"
    val contains = s"$base.contains"
    val entries = s"$base.entries"
    val length = s"$base.length"
  }

  object shapes {
    val base = s"${RuntimeApi.base}.shapes"

    val tpe = s"$base.type"
    val combine = s"$base.combine"
  }

  object traits {
    val base = s"${RuntimeApi.base}.traits"

    val schema = s"$base.schema"
    val tpe = s"$base.type"
  }

  object structs {
    val base = s"${RuntimeApi.base}.structs"

    val schema = s"$base.schema"
    val tpe = s"$base.type"
    val value = s"$base.value"
    val getPropertyType = s"$base.getPropertyType"
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

    object `lazy` {
      val base = s"${RuntimeApi.utils.base}.lazy"

      val of = s"$base.of"
    }

    object error {
      val base = s"${RuntimeApi.utils.base}.error"

      val ambiguousCall = s"$base.ambiguousCall"
      val emptyFit = s"$base.emptyFit"
      val missingImplementation = s"$base.missingImplementation"
    }
  }
}
