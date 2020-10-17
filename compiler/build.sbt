import sbt.Keys.{scalaSource, testOptions}

organization := "com.marcopennekamp"
name := "lore"
version := "0.1.0-SNAPSHOT"

scalaVersion := "2.13.3"
scalacOptions ++= Seq(
  "-feature",
  "-language:implicitConversions",
  "-language:postfixOps",
  "-language:higherKinds",
  "-language:reflectiveCalls",
  "-language:existentials",
  "-deprecation",
)

libraryDependencies ++= Seq(
  "org.scalaz" %% "scalaz-core" % "7.2.29",
  "com.chuusai" %% "shapeless" % "2.3.3",
  "org.scala-graph" %% "graph-core" % "1.13.2",
  "com.lihaoyi" %% "fastparse" % "2.1.3",
  "org.scalactic" %% "scalactic" % "3.1.0",
  "org.scalatest" %% "scalatest" % "3.1.0" % "test",
)

// Sonatype repos.
resolvers ++= Seq(
  Resolver.sonatypeRepo("releases"),
  Resolver.sonatypeRepo("snapshots"),
)

// ? types.
addCompilerPlugin("org.typelevel" %% "kind-projector" % "0.11.0" cross CrossVersion.full)

// Show time measurements of individual tests.
testOptions in Test += Tests.Argument("-oD")

// Shorten names of default src and test directories.
scalaSource in Compile := { (baseDirectory in Compile)(_ / "src") }.value
scalaSource in Test := { (baseDirectory in Test)(_ / "test") }.value

// Set lore.Lore as the main object. All other main functions are meant to be run via the IDE.
mainClass := Some("lore.compiler.Lore")

// Skip tests when using `sbt assembly`.
test in assembly := {}
