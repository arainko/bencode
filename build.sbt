val zioVersion = "1.0.4"

version := "0.1"

lazy val bencode = (project in file("."))
  .settings(
    name := "bencode",
    version := "0.0.5",
    scalaVersion := "2.13.4",
    scalacOptions ++= Seq("-Wconf:cat=unused:info", "-Ymacro-annotations"),
    semanticdbEnabled := true,
    semanticdbVersion := scalafixSemanticdb.revision,
    libraryDependencies ++= Seq(
      "org.typelevel" %% "cats-core"         % "2.3.1",
      "com.chuusai"    % "shapeless_2.13"    % "2.4.0-M1",
      "org.scodec"    %% "scodec-bits"       % "1.1.23",
      "dev.zio"       %% "zio"               % zioVersion,
      "dev.zio"       %% "zio-test"          % zioVersion % "test",
      "dev.zio"       %% "zio-test-sbt"      % zioVersion % "test",
      "dev.zio"       %% "zio-test-magnolia" % zioVersion % "test"
    )
  )
  .configs(IntegrationTest)
  .settings(Defaults.itSettings)

testFrameworks += new TestFramework("zio.test.sbt.ZTestFramework")

scalafixScalaBinaryVersion in ThisBuild := CrossVersion.binaryScalaVersion(scalaVersion.value)
ThisBuild / scalafixDependencies += "com.github.liancheng" %% "organize-imports" % "0.5.0"
