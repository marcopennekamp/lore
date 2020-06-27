package lore.compiler.phases.transpilation

object TranspiledNames {
  def temporaryVariable(name: String): String = s"lore_tmp_$name"
  def localVariable(loreName: String): String = s"lore_lv_$loreName"
}
