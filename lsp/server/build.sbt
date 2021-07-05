organization := "run.lore"
name := "lore-language-server"
version := "0.1.0-SNAPSHOT"

scalaVersion := "2.13.3"

libraryDependencies ++= Seq(
  "org.eclipse.lsp4j" % "org.eclipse.lsp4j" % "0.12.0",
)

// Shorten names of default src, resources, and test directories.
scalaSource in Compile := { (baseDirectory in Compile)(_ / "src") }.value
resourceDirectory in Compile := { (baseDirectory in Compile)(_ / "resources") }.value
scalaSource in Test := { (baseDirectory in Test)(_ / "test") }.value

mainClass := Some("lore.lsp.Main")
