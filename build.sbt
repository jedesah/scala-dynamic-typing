name := "Typecheck At Runtime"

organization := "com.github.jedesah"

version := "0.1.0-SNAPSHOT"

scalaVersion := "2.11.6"

libraryDependencies ++= Seq(
  "org.scala-lang" % "scala-reflect" % scalaVersion.value,
  "org.specs2" %% "specs2-core" % "3.4" % "test",
  "org.specs2" %% "specs2-matcher-extra" % "3.4" % "test"
)

scalacOptions in Test ++= Seq("-Yrangepos")

resolvers += "scalaz-bintray" at "http://dl.bintray.com/scalaz/releases"

initialCommands := "import com.github.jedesah.typelevel.test._"
