name := "lore"

version := "1.0"

scalaVersion := "2.11.8"

libraryDependencies += "com.slamdata" % "matryoshka-core_2.11" % "0.18.2"
libraryDependencies += "org.scala-lang.modules" %% "scala-parser-combinators" % "1.0.5"

scalaSource in Compile := { (baseDirectory in Compile)(_ / "src") }.value
