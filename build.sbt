import xerial.sbt.Sonatype._

name := "bencode"
organization := "io.github.arainko"
licenses := Seq("APL2" -> url("https://www.apache.org/licenses/LICENSE-2.0.txt"))
description := "A bencode library with typesafe schema derivation"

publishMavenStyle := true
sonatypeProjectHosting := Some(GitHubHosting("arainko", "bencode", "aleksander.rainko99@gmail.com"))
sonatypeRepository := "https://s01.oss.sonatype.org/service/local"
sonatypeCredentialHost := "s01.oss.sonatype.org"
publishTo := sonatypePublishToBundle.value

val zioVersion = "1.0.4"

lazy val bencode = (project in file("."))
  .settings(
    name := "bencode",
    scalaVersion := "2.13.4",
    scalacOptions ++= Seq("-Wconf:cat=unused:info", "-Ymacro-annotations"),
    semanticdbEnabled := true,
    semanticdbVersion := scalafixSemanticdb.revision,
    libraryDependencies ++= Seq(
      "org.typelevel" %% "cats-core"         % "2.3.1",
      "com.chuusai"   %% "shapeless"         % "2.4.0-M1",
      "org.scodec"    %% "scodec-bits"       % "1.1.23",
      "dev.zio"       %% "zio"               % zioVersion % "test",
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
