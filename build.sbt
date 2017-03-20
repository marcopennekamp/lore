name := "lore"

version := "1.0"

scalaVersion := "2.11.8"

libraryDependencies ++= Seq(
  "com.slamdata" % "matryoshka-core_2.11" % "0.18.2",
  "org.scala-lang.modules" %% "scala-parser-combinators" % "1.0.5",
  "com.lihaoyi" %% "fastparse" % "0.4.2"
)

scalaSource in Compile := { (baseDirectory in Compile)(_ / "src") }.value
