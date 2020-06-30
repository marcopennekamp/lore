package lore.compiler.phases.transpilation

object LoreApi {
  val varTypes = "Lore.types"
  val varValues = "Lore.values"
  val varUtils = "Lore.utils"
  val varTinyMap = s"$varUtils.tinyMap"
  val varTinySet = s"$varUtils.tinySet"
  val varList = s"$varValues.list"
  val varTuple = s"$varValues.tuple"
  val varObject = s"$varValues.object"
}
