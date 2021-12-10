name := "AOC2019"

//version := "0.1"

scalaVersion := "3.1.0"

scalacOptions ++= Seq(
  "-unchecked",
  "-deprecation",
  "utf8",
  "-feature",
  "literal-types",
  "-Xfatal-warnings",
  "-Yexplicit-nulls",
  "-source:future"
)

//addCompilerPlugin("org.typelevel" %% "kind-projector" % "0.13.0" cross CrossVersion.full)

val zioVersion = "2.0.0-M6-2"
val catsVersion = "2.6.1"
val catsEffectVersion = "3.2.1"

libraryDependencies ++= Seq(
  "dev.zio"       %% "zio"              % zioVersion,
  "dev.zio"       %% "zio-nio"          % "1.0.0-RC11",
  "dev.zio"       %% "zio-streams"      % zioVersion,
//  "org.typelevel" %% "cats-core"        % catsVersion,
//  "org.typelevel" %% "cats-effect"      % catsEffectVersion,
//  "dev.zio"       %% "zio-interop-cats" % "3.1.1.0",
  "dev.zio"       %% "zio-test"         % zioVersion    % "test",
  "dev.zio"       %% "zio-test-sbt"     % zioVersion    % "test"
)

testFrameworks ++= Seq(new TestFramework("zio.test.sbt.ZTestFramework"))
