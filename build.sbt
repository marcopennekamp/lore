name := "lore"

version := "0.1.0"

scalaVersion := "2.11.8"
scalacOptions ++= Seq(
  "-feature",
  "-language:implicitConversions",
  "-language:postfixOps",
  "-language:higherKinds",
  "-language:reflectiveCalls"
)

libraryDependencies ++= Seq(
  "org.scalaz" %% "scalaz-core" % "7.2.20",
  "com.lihaoyi" %% "fastparse" % "1.0.0"
)

// ? types.
resolvers += Resolver.sonatypeRepo("releases")
addCompilerPlugin("org.spire-math" %% "kind-projector" % "0.9.3")

scalaSource in Compile := { (baseDirectory in Compile)(_ / "src") }.value
