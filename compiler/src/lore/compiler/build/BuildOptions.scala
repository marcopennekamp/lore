package lore.compiler.build

import lore.compiler.core.CompilerOptions

import java.nio.file.Path

case class BuildOptions(
  /**
    * The source files to compile, relative to the current working directory. Specifying a directory will compile all
    * files in the directory.
    */
  sources: Vector[Path] = Vector.empty,

  /**
    * The location of the Lore SDK. It must contain the Pyramid standard library in a directory called `pyramid/` and
    * the runtime in a directory called `runtime/`.
    */
  sdk: Path = Path.of(BuildOptions.defaultSdk),

  /**
    * The file that the generated Javascript code is written to, the default being `lore-program.js`.
    */
  target: Path = Path.of(BuildOptions.defaultTarget),

  /**
    * Whether the generated Javascript code should be beautified with prettier. This incurs an additional compile-time
    * cost of a few hundred milliseconds.
    *
    * TODO (assembly): Remove this.
    */
  enablePrettier: Boolean = true,

  /**
    * CompilerOptions are used in the internals of the compiler, which are file- and build-agnostic.
    */
  compilerOptions: CompilerOptions = CompilerOptions(),
) {
  def withSources(sources: Path*): BuildOptions = copy(sources = this.sources ++ sources)
}

object BuildOptions {
  val defaultSdk = "."
  val defaultTarget = "lore-program.js"
}
