
// shadow sbt-scalajs' crossProject and CrossType from Scala.js 0.6.x
import sbtcrossproject.CrossPlugin.autoImport.{crossProject, CrossType}


val sharedSettings = Seq(scalaVersion := "2.12.7")



lazy val nmmlogic = crossProject(JSPlatform, JVMPlatform)
  .crossType(CrossType.Pure)
  .in(file("nmmlogic"))
  .settings(
    name := "nmmlogic",
    organization := "com.ofenbeck",
    version := "0.1.1"
  )
  .settings(
    libraryDependencies += "org.scalacheck" %% "scalacheck" % "1.14.0" % "test"
  )