import sbt._
import sbtcrossproject.CrossPlugin.autoImport.crossProject
import sbtcrossproject.CrossType

resolvers += "Sonatype Releases" at "https://oss.sonatype.org/content/repositories/releases/"
resolvers += Resolver.bintrayRepo("edadma", "maven")

// set fullClasspath in Compile in console += Attributed.blank(file("C:/Program Files/Java/jdk1.8.0_112/lib/tools.jar"))

lazy val commonSettings = Seq(
  organization := "com.github.daniil-timofeev",
  version := "0.2.0-SNAPSHOT",
  scalaVersion := "2.12.5",
  crossScalaVersions := Seq("2.12.5", "2.11.12")
)

lazy val commonDependencies = libraryDependencies ++= Seq(
      "com.outr" %%% "scribe" % "2.3.3",
      "org.typelevel" %%% "cats-core" % "1.1.0",
      "org.scalacheck" %%% "scalacheck" % "1.13.4" % "test",
      "org.specs2" %%% "specs2-core" %  "4.2.0"  % "test")

lazy val piecewise = crossProject(JVMPlatform, JSPlatform)
  .crossType(CrossType.Pure)
  .in(file("piecewise"))
  .settings(
    name := "gridsplines-piecewise",
    commonDependencies,
    commonSettings
  )

lazy val piecewiseJVM = piecewise.jvm
lazy val piecewiseJS = piecewise.js

lazy val approximation = crossProject(JVMPlatform)
  .crossType(CrossType.Pure)
  .in(file("approximation"))
  .jvmSettings(
    name := "gridsplines-approximation",
    commonDependencies,
    libraryDependencies ++= Seq("org.apache.commons" % "commons-math3" % "3.6.1" % "test"),
    commonSettings
  ).dependsOn(piecewise)

lazy val approximationJVM = approximation.jvm

lazy val gridsplines = crossProject(JVMPlatform)
  .crossType(CrossType.Pure)
  .in(file("."))
  .aggregate(piecewise, approximation)
  .settings(
    name := "gridsplines",
    commonSettings
  )

lazy val gridsplinesJVM = gridsplines.jvm


scalacOptions ++= Seq(
  "-Ypartial-unification",
  "-language:existentials",
  "-language:higherKinds",
  "-language:implicitConversions"
)



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