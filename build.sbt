import sbt._
organization := "org.msuce.daniil"

name := "gridsplines"


version := "0.2.0-SNAPSHOT"

scalaVersion := "2.12.3"

crossScalaVersions := Seq("2.12.3", "2.11.11")
resolvers += "Sonatype Releases" at "https://oss.sonatype.org/content/repositories/releases/"
resolvers += Resolver.bintrayRepo("edadma", "maven")

// set fullClasspath in Compile in console += Attributed.blank(file("C:/Program Files/Java/jdk1.8.0_112/lib/tools.jar"))

libraryDependencies ++= "org.slf4j" % "slf4j-api" % "1.7.22" ::
                        "ch.qos.logback" % "logback-core" % "1.1.8" ::
                        "ch.qos.logback" % "logback-classic" % "1.1.8" ::
                        "com.twitter" %% "algebird-core" % "0.13.4-SNAPSHOT" ::
                        "org.scalacheck" %% "scalacheck" % "1.13.4" % "test" ::
                        "org.specs2" %% "specs2-core" % "3.8.9" % "test" :: Nil

scalacOptions in Test ++= Seq("-Yrangepos")




