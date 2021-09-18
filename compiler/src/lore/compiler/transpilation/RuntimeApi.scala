package lore.compiler.transpilation

import lore.compiler.semantics.NamePath
import lore.compiler.target.Target
import lore.compiler.target.Target.TargetExpression
import lore.compiler.target.TargetDsl._
import lore.compiler.types.TypeVariable.Variance

//noinspection TypeAnnotation
object RuntimeApi {
  private val base = "Lore".asVariable
  private def named(name: String)(implicit base: TargetExpression) = base.prop(name)

  object types {
    implicit val base = named("types")(RuntimeApi.base)

    val any = named("any")
    val nothing = named("nothing")
    val real = named("real")
    val int = named("int")
    val boolean = named("boolean")
    val string = named("string")

    def variable(index: Int, lowerBound: TargetExpression, upperBound: TargetExpression, variance: Variance, fullName: String) = {
      named("variable").call(Target.IntLiteral(index), lowerBound, upperBound, named("variance").prop(variance.toString), fullName.asLiteral)
    }

    def fitsMonomorphic(t1: TargetExpression, t2: TargetExpression) = named("fitsMonomorphic").call(t1, t2)
    def fitsPolymorphic(t1: TargetExpression, t2: TargetExpression, variables: TargetExpression) = named("fitsPolymorphic").call(t1, t2, variables)
    def typeOf(value: TargetExpression) = named("typeOf").call(value)

    /**
      * Accesses run-time type attributes directly to avoid a litany of function calls when accessing type paths,
      * unless run-time support is required (which is the case for shapes and trait type arguments).
      */
    object typePaths {
      implicit val base = named("typePaths")(RuntimeApi.types.base)

      def tupleElement(tpe: TargetExpression, index: Int) = tpe.prop("types").element(Target.IntLiteral(index))
      def functionInput(tpe: TargetExpression) = tpe.prop("input")
      def functionOutput(tpe: TargetExpression) = tpe.prop("output")
      def listElement(tpe: TargetExpression) = tpe.prop("element")
      def mapKey(tpe: TargetExpression) = tpe.prop("key")
      def mapValue(tpe: TargetExpression) = tpe.prop("value")
      def shapeProperty(tpe: TargetExpression, name: String) = named("shapeProperty").call(tpe, Target.StringLiteral(name))
      def structTypeArgument(tpe: TargetExpression, index: Int) = tpe.prop("typeArguments").element(Target.IntLiteral(index))
      def typeArgument(tpe: TargetExpression, schema: TargetExpression, index: Int) = named("typeArgument").call(tpe, schema, Target.IntLiteral(index))
    }

    object introspection {
      implicit val base = named("introspection")(RuntimeApi.types.base)

      def initialize(traitType: TargetExpression) = named("initialize").call(traitType)
    }
  }

  object sums {
    implicit val base = named("sums")(RuntimeApi.base)

    def tpe(types: Vector[TargetExpression]) = named("type").call(Target.List(types))
    def simplified(types: Vector[TargetExpression]) = named("simplified").call(Target.List(types))
  }

  object intersections {
    implicit val base = named("intersections")(RuntimeApi.base)

    def tpe(types: Vector[TargetExpression]) = named("type").call(Target.List(types))
    def simplified(types: Vector[TargetExpression]) = named("simplified").call(Target.List(types))
  }

  object tuples {
    implicit val base = named("tuples")(RuntimeApi.base)

    def tpe(types: TargetExpression): Target.Call = named("type").call(types)
    def tpe(types: Vector[TargetExpression]): Target.Call = tpe(Target.List(types))
    def unhashedType(types: TargetExpression): Target.Call = named("unhashedType").call(types)
    def unhashedType(types: Vector[TargetExpression]): Target.Call = unhashedType(Target.List(types))
    val unitType = named("unitType")
    def value(elements: Vector[TargetExpression]) = named("value").call(Target.List(elements))
    val unitValue = named("unitValue")
  }

  object functions {
    implicit val base = named("functions")(RuntimeApi.base)

    def tpe(input: TargetExpression, output: TargetExpression) = named("type").call(input, output)
    def value(callable: TargetExpression, tpe: TargetExpression) = named("value").call(callable, tpe)
    def call(function: TargetExpression, arguments: Vector[TargetExpression]) = Target.Call(named("call"), function +: arguments)
  }

  object lists {
    implicit val base = named("lists")(RuntimeApi.base)

    def tpe(element: TargetExpression) = named("type").call(element)
    def value(values: Vector[TargetExpression], tpe: TargetExpression) = named("value").call(Target.List(values), tpe)
    def append(list: TargetExpression, element: TargetExpression, tpe: TargetExpression) = named("append").call(list, element, tpe)
    def appendUntyped(list: TargetExpression, element: TargetExpression) = named("appendUntyped").call(list, element)
  }

  object maps {
    implicit val base = named("maps")(RuntimeApi.base)

    def tpe(key: TargetExpression, value: TargetExpression) = named("type").call(key, value)
    def value(entries: Vector[TargetExpression], tpe: TargetExpression, hash: TargetExpression, equals: TargetExpression) = {
      named("value").call(Target.List(entries), tpe, hash, equals)
    }
    def entries(map: TargetExpression) = named("entries").call(map)
  }

  object shapes {
    implicit val base = named("shapes")(RuntimeApi.base)

    def tpe(propertyTypes: TargetExpression) = named("type").call(propertyTypes)
    def value(properties: Target.Dictionary) = named("value").call(properties)
  }

  object symbols {
    implicit val base = named("symbols")(RuntimeApi.base)

    def tpe(name: String) = named("type").call(name.asLiteral)
    def value(tpe: Target.Variable) = named("value").call(tpe)
  }

  object traits {
    implicit val base = named("traits")(RuntimeApi.base)

    def schema(
      name: NamePath,
      typeParameters: Vector[TargetExpression],
      supertraits: Vector[TargetExpression],
      hasMultipleParameterizedInheritance: Boolean,
      inheritedShapeType: TargetExpression,
    ) = {
      named("schema").call(
        name.toString.asLiteral,
        Target.List(typeParameters),
        Target.List(supertraits),
        Target.BooleanLiteral(hasMultipleParameterizedInheritance),
        inheritedShapeType
      )
    }
    def tpe(schema: TargetExpression, typeArguments: TargetExpression) = named("type").call(schema, typeArguments)
  }

  object structs {
    implicit val base = named("structs")(RuntimeApi.base)

    def schema(
      name: NamePath,
      typeParameters: Vector[TargetExpression],
      supertraits: Vector[TargetExpression],
      hasMultipleParameterizedInheritance: Boolean,
      propertyTypes: TargetExpression,
      propertyOrder: Vector[String],
      openPropertyOrder: Vector[String],
    ) = {
      named("schema").call(
        name.toString.asLiteral,
        Target.List(typeParameters),
        Target.List(supertraits),
        Target.BooleanLiteral(hasMultipleParameterizedInheritance),
        propertyTypes,
        Target.List(propertyOrder.map(_.asLiteral)),
        Target.List(openPropertyOrder.map(_.asLiteral)),
      )
    }
    def tpe(schema: TargetExpression, typeArguments: TargetExpression, propertyTypes: TargetExpression) = {
      named("type").call(schema, typeArguments, propertyTypes)
    }
    def value(properties: TargetExpression, tpe: TargetExpression) = named("value").call(properties, tpe)
    def getConstructor(schema: TargetExpression, typeArguments: TargetExpression, construct: TargetExpression) = {
      named("getConstructor").call(schema, typeArguments, construct)
    }
  }

  object utils {
    implicit val base = named("utils")(RuntimeApi.base)

    object tinyMap {
      implicit val base = named("tinyMap")(RuntimeApi.utils.base)

      def get(map: TargetExpression, key: TargetExpression) = named("get").call(map, key)
    }

    object typeMap {
      implicit val base = named("typeMap")(RuntimeApi.utils.base)

      def create() = named("create").call()
    }

    object `lazy` {
      implicit val base = named("lazy")(RuntimeApi.utils.base)

      def of(value: TargetExpression) = named("of").call(Target.Lambda(Vector.empty, value))
    }

    object error {
      implicit val base = named("error")(RuntimeApi.utils.base)

      def ambiguousCall(functionName: NamePath, inputType: TargetExpression) = {
        named("ambiguousCall").call(functionName.toString.asLiteral, inputType)
      }
      def emptyFit(functionName: NamePath, inputType: TargetExpression) = {
        named("emptyFit").call(functionName.toString.asLiteral, inputType)
      }
      def missingImplementation(functionName: NamePath, parameterType: TargetExpression, argumentType: TargetExpression) = {
        named("missingImplementation").call(functionName.toString.asLiteral, parameterType, argumentType)
      }
    }
  }

  object io {
    implicit val base = named("io")(RuntimeApi.base)

    def println(value: TargetExpression) = named("println").call(value)
  }
}
