import sbt._
name := "SSpline"

version := "1.0"

scalaVersion := "2.12.1"

// set fullClasspath in Compile in console += Attributed.blank(file("C:/Program Files/Java/jdk1.8.0_112/lib/tools.jar"))

libraryDependencies ++= "com.twitter" %% "algebird-core" % "0.12.4" ::
                        "org.scalacheck" %% "scalacheck" % "1.13.4" % "test" :: Nil
import org.ensime.EnsimeCoursierKeys._
ensimeServerVersion in ThisBuild := "2.0.0-SNAPSHOT"
