package lore.compiler.phases.transpilation

import lore.compiler.core.Compilation.ToCompilationExtension
import lore.compiler.core.{Compilation, CompilationException}
import lore.compiler.types.{DeclaredType, StructType, TraitType}

object DeclaredTypeTranspiler {

  /**
    * Transpiles the given declared type to its representation. Any types this type depends on must be transpiled
    * before this type is transpiled!
    */
  def transpile(tpe: DeclaredType): Compilation[String] = {
    tpe match {
      case structType: StructType => transpileStructType(structType)
      case traitType: TraitType => transpileTraitType(traitType)
      case _ => throw CompilationException(s"Unknown declared type $tpe.")
    }
  }

  /**
    * Transpiles a struct type to its Javascript representation.
    *
    * A transpiled struct type consists of three parts:
    *   1. The type schema which saves the information (names, ownedBy, etc.) common to all instances of the struct
    *      type, regardless of which component types are actually instantiated.
    *   2. The "newtype" function which can be used to create a new instance of the struct type. This is needed
    *      because struct types are dependent on their actual components' types at run-time. Hence, for each struct
    *      instantiated, we also have to create a new type.
    *   3. The archetypal compile-time type instance, which is a struct type instantiated with the component types
    *      that it owns at compile-time. This type is used when being referred to in multiple dispatch and other
    *      static uses.
    */
  private def transpileStructType(tpe: StructType): Compilation[String] = {
    val varStructType = TranspiledNames.declaredType(tpe)
    val varDeclaredSupertypes = tpe.declaredSupertypes.map(TranspiledNames.declaredType)
    // TODO: Support componentTypes. To properly support components, we will have to instantiate a new type for each
    //       new object, because we need the concrete type of the actual components. (This is in addition to the
    //       "golden standard" struct type which always contains the compile-time component types). So in addition
    //       to the varStructType below, which is the golden standard type, we have to transpile an instantiation
    //       function that creates such a struct type for a given list of run-time component types.
    // TODO: Support ownedBy types.
    // TODO: Support isEntity.




    s"""const $varStructType = ${RuntimeApi.types.structType}(
       |  '${tpe.name}',
       |  [${varDeclaredSupertypes.mkString(", ")}],
       |  [],
       |  undefined,
       |  false,
       |);""".stripMargin.compiled
  }

  /**
    * Transpiles a trait type to its Javascript representation.
    *
    * To preserve symmetry with struct types, trait types also have a schema and a newtype function. These are not
    * strictly necessary, but ease the burden on runtime definitions.
    *
    */
  private def transpileTraitType(tpe: TraitType): Compilation[String] = {
    val varTraitType = TranspiledNames.declaredType(tpe)
    s"const $varTraitType = { };".compiled // TODO: Implement.
  }

}
