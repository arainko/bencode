Global / onChangedBuildSource := ReloadOnSourceChanges
ThisBuild / versionScheme := Some("early-semver")
ThisBuild / organization := "io.github.arainko"
ThisBuild / homepage := Some(url("https://github.com/arainko/bencode"))
ThisBuild / licenses := List("Apache-2.0" -> url("http://www.apache.org/licenses/LICENSE-2.0"))
ThisBuild / developers := List(
  Developer(
    "arainko",
    "Aleksander Rainko",
    "aleksander.rainko99@gmail.com",
    url("https://github.com/arainko")
  )
)
ThisBuild / scalaVersion := "3.2.1"
ThisBuild / scalafixDependencies += "com.github.liancheng" %% "organize-imports" % "0.6.0"

name := "bencode"
sonatypeRepository := "https://s01.oss.sonatype.org/service/local"
sonatypeCredentialHost := "s01.oss.sonatype.org"

lazy val root =
  project
    .in(file("."))
    .settings(publish / skip := true)
    .aggregate(bencode)

lazy val bencode =
  project
    .in(file("bencode"))
    .settings(
      scalacOptions ++= List("-Xcheck-macros", "-no-indent", "-old-syntax", "-Xfatal-warnings"),
      libraryDependencies += "org.scalameta" %% "munit" % "1.0.0-M6" % Test
    )