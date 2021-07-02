package lore.compiler.cli

import lore.compiler.core.CompilerOptions

import java.nio.file.Path

case class CliOptions(
  /**
    * The source files to compile, relative to the base directory. Specifying a directory will compile all files in the
    * directory.
    */
  sources: Vector[Path] = Vector(Path.of("pyramid")),

  /**
    * The base directory acts as the root for all source files. It must contain the Pyramid standard library in a
    * directory called `pyramid/` and the runtime in a directory called `runtime/`.
    */
  baseDirectory: Path = Path.of(CompilerOptions.defaultBaseDirectory),

  /**
    * The file that the generated Javascript code is written to, the default being `lore-program.js`.
    */
  outputFile: Path = Path.of(CompilerOptions.defaultOutputFileName),

  /**
    * CompilerOptions are used in the internals of the compiler and .
    */
  compilerOptions: CompilerOptions = CompilerOptions(),
) {
  def withSources(sources: Path*): CliOptions = copy(sources = this.sources ++ sources)
}
