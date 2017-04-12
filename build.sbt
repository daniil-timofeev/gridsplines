import sbt._
name := "GridSplines"

version := "0.01"

scalaVersion := "2.12.1"

resolvers += "Sonatype Releases" at "https://oss.sonatype.org/content/repositories/releases/"
resolvers += Resolver.bintrayRepo("edadma", "maven")

// set fullClasspath in Compile in console += Attributed.blank(file("C:/Program Files/Java/jdk1.8.0_112/lib/tools.jar"))

libraryDependencies ++= "org.slf4j" % "slf4j-api" % "1.7.22" ::
                        "ch.qos.logback" % "logback-core" % "1.1.8" ::
                        "ch.qos.logback" % "logback-classic" % "1.1.8" ::
                        "com.twitter" %% "algebird-core" % "0.12.4" ::
                        "org.scalacheck" %% "scalacheck" % "1.13.4" % "test"  ::
                        "com.lihaoyi" %% "ammonite-ops" % "0.8.1" ::
                        "xyz.hyperreal" %% "b-tree" % "0.3" :: Nil




