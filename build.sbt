name := "half-commit-kv"

version := "0.1"

scalaVersion := "2.12.7"

libraryDependencies += "com.arangodb" % "arangodb-java-driver" % "5.0.0"

libraryDependencies ++= Seq(
  "com.github.pheymann" %% "typedapi-client" % "0.2.0", 
  "com.github.pheymann" %% "typedapi-server" % "0.2.0"
)

libraryDependencies ++= Seq(
  "io.circe" %% "circe-core" % "0.9.1",
  "io.circe" %% "circe-parser" % "0.9.1",
  "io.circe" %% "circe-generic" % "0.9.1",
  "io.circe" %% "circe-generic-extras" % "0.9.1"
)

libraryDependencies += "org.specs2" %% "specs2-core" % "4.3.4" % Test

libraryDependencies ++= Seq(
  "org.http4s" %% "http4s-dsl" % "0.19.0-M3",
  "org.http4s" %% "http4s-blaze-server" % "0.19.0-M3",
  "org.http4s" %% "http4s-blaze-client" % "0.19.0-M3",
  "org.http4s" %% "http4s-circe" % "0.19.0-M3"
)

scalacOptions := Seq(
  "-deprecation",
  "-encoding", "utf-8",
  "-explaintypes",
  "-feature",
  "-unchecked",
  "-language:existentials", // Existential types (besides wildcard types) can be written and inferred
  "-language:experimental.macros", // Allow macro definition (besides implementation and application)
  "-language:higherKinds", // Allow higher-kinded types
  "-language:implicitConversions", // Allow definition of implicit functions called views
  "-Xfatal-warnings",
  "-Xfuture",
  "-Xlint:inaccessible",
  "-Xlint:infer-any",
  "-Xlint:missing-interpolator",
  "-Xlint:option-implicit",
  "-Xlint:type-parameter-shadow",
  "-Xlint:unsound-match",
  "-Ywarn-dead-code",
  "-Ywarn-inaccessible",
  "-Ywarn-infer-any",
  "-Ypartial-unification",
  "-Ywarn-numeric-widen",
  //   "-Ywarn-unused:implicits", -> get errors for implicit evidence
  "-Ywarn-unused:imports",
  //   "-Ywarn-unused:locals",
  "-Ywarn-unused:privates"
)