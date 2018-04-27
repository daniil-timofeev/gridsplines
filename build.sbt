import sbt._
organization := "com.github.daniil-timofeev"

name := "gridsplines"


version := "0.2.0-SNAPSHOT"

scalaVersion := "2.12.5"

crossScalaVersions := Seq("2.12.5", "2.11.11")
resolvers += "Sonatype Releases" at "https://oss.sonatype.org/content/repositories/releases/"
resolvers += Resolver.bintrayRepo("edadma", "maven")

// set fullClasspath in Compile in console += Attributed.blank(file("C:/Program Files/Java/jdk1.8.0_112/lib/tools.jar"))

libraryDependencies ++= "org.slf4j" % "slf4j-api" % "1.7.22" ::
                        "ch.qos.logback" % "logback-core" % "1.1.8" ::
                        "ch.qos.logback" % "logback-classic" % "1.1.8" ::
                        "com.twitter" %% "algebird-core" % "0.13.0" ::
                        "org.scalacheck" %% "scalacheck" % "1.13.4" % "test" ::
                        "org.specs2" %% "specs2-core" % "3.8.9" % "test" :: Nil
         
scalacOptions in Test ++= Seq("-Yrangepos")

useGpg := true

pomIncludeRepository := { _ => false }

licenses := Seq("Apache License 2.0" -> url("http://www.apache.org/licenses/"))

homepage := Some(url("https://github.com/daniil-timofeev/gridsplines"))

scmInfo := Some(
  ScmInfo(
    url("https://github.com/daniil-timofeev/gridsplines"),
    "https://github.com/daniil-timofeev/gridsplines.git"
  )
)

developers := List(
  Developer(
    id    = "daniil-timofeev",
    name  = "Daniil Timofeev",
    email = "daniil@fastmail.fm",
    url   = url("https://github.com/daniil-timofeev/gridsplines")
  )
)

publishMavenStyle := true

sonatypeProfileName := "(com.github.daniil-timofeev)"


publishArtifact in Test := false

// Add sonatype repository settings
publishTo := {
  val nexus = "https://oss.sonatype.org/"
  if (isSnapshot.value)
    Some("snapshots" at nexus + "content/repositories/snapshots")
  else
    Some("releases" at nexus + "service/local/staging/deploy/maven2")
}