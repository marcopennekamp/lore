name := "lore"

version := "0.1.0"

scalaVersion := "2.13.1"
scalacOptions ++= Seq(
  "-feature",
  "-language:implicitConversions",
  "-language:postfixOps",
  "-language:higherKinds",
  "-language:reflectiveCalls",
  "-language:existentials",
  "-deprecation"
)

libraryDependencies ++= Seq(
  "org.scalaz" %% "scalaz-core" % "7.2.29",
  "org.scala-graph" %% "graph-core" % "1.13.2",
  "com.lihaoyi" %% "fastparse" % "2.1.3",
  "org.scalactic" %% "scalactic" % "3.1.0",
  "org.scalatest" %% "scalatest" % "3.1.0" % "test"
)

// ? types.
resolvers += Resolver.sonatypeRepo("releases")
addCompilerPlugin("org.typelevel" %% "kind-projector" % "0.11.0" cross CrossVersion.full)

scalaSource in Compile := { (baseDirectory in Compile)(_ / "src") }.value
scalaSource in Test := { (baseDirectory in Test)(_ / "test") }.value

// Show time measurements of individual tests.
testOptions in Test += Tests.Argument("-oD")

// Set lore.Lore as the main object. All other main functions are meant to be run via the IDE.
mainClass in (Compile, run) := Some("lore.Lore")
