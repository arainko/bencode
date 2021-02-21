name := "bencode"

val zioVersion = "1.0.4"

version := "0.1"

scalaVersion := "2.13.4"

inThisBuild(
  List(
    scalaVersion := "2.13.4",
    semanticdbEnabled := true,
    semanticdbVersion := scalafixSemanticdb.revision,
    scalacOptions += "-Wunused:imports"
  )
)

libraryDependencies += "org.typelevel" %% "cats-core" % "2.3.1"
libraryDependencies += "com.chuusai" % "shapeless_2.13" % "2.4.0-M1"

libraryDependencies ++= Seq(
  "dev.zio" %% "zio" % zioVersion,
  "dev.zio" %% "zio-test" % zioVersion % "test",
  "dev.zio" %% "zio-test-sbt" % zioVersion % "test",
  "dev.zio" %% "zio-test-magnolia" % zioVersion % "test"
)
testFrameworks += new TestFramework("zio.test.sbt.ZTestFramework")
