name := "AOC2019"

version := "0.1"

scalaVersion := "2.13.1"

addCompilerPlugin("org.typelevel" %% "kind-projector" % "0.11.0" cross CrossVersion.full)

val zioVersion = "1.0.0-RC17"
val catsVersion = "2.0.0"

libraryDependencies ++= Seq(
  "dev.zio"       %% "zio"              % zioVersion,
  "dev.zio"       %% "zio-nio"          % "0.4.0",
  "dev.zio"       %% "zio-streams"      % zioVersion,
  "org.typelevel" %% "cats-core"        % catsVersion,
  "org.typelevel" %% "cats-effect"      % catsVersion,
  "dev.zio"       %% "zio-interop-cats" % "2.0.0.0-RC10",
  "dev.zio"       %% "zio-test"         % zioVersion    % "test",
  "dev.zio"       %% "zio-test-sbt"     % zioVersion    % "test"
)

testFrameworks ++= Seq(new TestFramework("zio.test.sbt.ZTestFramework"))
