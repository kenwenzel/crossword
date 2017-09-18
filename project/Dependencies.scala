import sbt._

object Dependencies {
  lazy val scalaTest = "org.scalatest" %% "scalatest" % "3.0.1"
  lazy val scalaXml = "org.scala-lang.modules" % "scala-xml_2.12" % "1.0.6"
  lazy val watchmaker = "org.uncommons.watchmaker" % "watchmaker-framework" % "0.7.1"

  lazy val allDeps = Seq(watchmaker, scalaXml, scalaTest % Test)
}
