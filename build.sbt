name := "code-reuse-fp"

version := "0.1"

scalaVersion := "2.12.8"

libraryDependencies += "org.typelevel" %% "cats-effect" % "1.2.0"
libraryDependencies += "org.typelevel" %% "cats-mtl-core" % "0.4.0"

addCompilerPlugin("org.spire-math" %% "kind-projector" % "0.9.8")

scalacOptions += "-feature"
scalacOptions += "-Ypartial-unification"
scalacOptions += "-language:higherKinds"

