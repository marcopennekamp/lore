package lore.compiler.target

/**
  * Signifies a language entity that is known by some identifier in the target representation.
  */
trait TargetIdentifiable {
  def targetVariable: Target.Variable
}
