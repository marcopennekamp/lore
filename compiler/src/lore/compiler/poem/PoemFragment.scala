package lore.compiler.poem

case class PoemFragment(
  schemas: Vector[PoemSchema],
  globalVariables: Vector[PoemGlobalVariable],
  functions: Vector[PoemFunction],
  specs: Vector[PoemSpec],
)
