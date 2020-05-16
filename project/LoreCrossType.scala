import java.io.File

import sbt._

import sbtcrossproject.{CrossType, Platform}

object LoreCrossType extends CrossType {
  @deprecated("use projectDir(crossBase: File, platform: Platform): File",
    "0.1.0")
  def projectDir(crossBase: File, projectType: String): File = projectType match {
    case "jvm" => crossBase / "compiler"
    case "js" => crossBase / "runtime"
    case _ => ???
  }

  def projectDir(crossBase: File, platform: Platform): File = projectDir(crossBase, platform.identifier)

  def sharedSrcDir(projectBase: File, conf: String): Option[File] = {
    System.out.println(projectBase.getParentFile)
    Some(projectBase.getParentFile / "common" / "src")
  }
}
