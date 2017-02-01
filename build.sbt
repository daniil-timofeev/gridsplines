import sbt._

name := "SSpline"

version := "1.0"

scalaVersion := "2.12.1"

// set fullClasspath in Compile in console += Attributed.blank(file("C:/Program Files/Java/jdk1.8.0_112/lib/tools.jar"))

libraryDependencies += "com.twitter" %% "algebird-core" % "0.12.4"
