import sbt.Keys.{scalaSource, testOptions}

val sharedSettings = Seq(
  organization := "com.marcopennekamp",
  name := "lore",
  version := "0.1.0-SNAPSHOT",

  scalaVersion := "2.13.1",
  scalacOptions ++= Seq(
    "-feature",
    "-language:implicitConversions",
    "-language:postfixOps",
    "-language:higherKinds",
    "-language:reflectiveCalls",
    "-language:existentials",
    "-deprecation",
  ),

  libraryDependencies ++= Seq(
    "org.scalaz" %% "scalaz-core" % "7.2.29",
    "com.chuusai" %% "shapeless" % "2.3.3",
    "org.scala-graph" %% "graph-core" % "1.13.2",
    "com.lihaoyi" %% "fastparse" % "2.1.3",
    "org.scalactic" %% "scalactic" % "3.1.0",
    "org.scalatest" %% "scalatest" % "3.1.0" % "test",
  ),

  // Sonatype repos.
  resolvers ++= Seq(
    Resolver.sonatypeRepo("releases"),
    Resolver.sonatypeRepo("snapshots"),
  ),

  // ? types.
  addCompilerPlugin("org.typelevel" %% "kind-projector" % "0.11.0" cross CrossVersion.full),

  // Show time measurements of individual tests.
  testOptions in Test += Tests.Argument("-oD"),
)

lazy val lore =
  crossProject(JVMPlatform, JSPlatform)
    .crossType(LoreCrossType)
    .in(file("."))
    .settings(sharedSettings)
    .jvmSettings(
      scalaSource in Compile := { (baseDirectory in Compile)(_ / "src") }.value,
      scalaSource in Test := { (baseDirectory in Test)(_ / "test") }.value,

      // Set lore.Lore as the main object. All other main functions are meant to be run via the IDE.
      mainClass in (Compile, run) := Some("lore.compiler.Lore"),
    )
    .jvmConfigure(_.withId("compiler"))
    .jsSettings(
      scalaSource in Compile := { (baseDirectory in Compile)(_ / "src") }.value,
      scalaSource in Test := { (baseDirectory in Test)(_ / "test") }.value,
      scalaJSLinkerConfig ~= { _.withModuleKind(ModuleKind.ESModule) },
    )
    .jsConfigure(_.withId("runtime"))

lazy val root = project.in(file(".")).aggregate(lore.js, lore.jvm)
