import sbt._
import com.typesafe.sbt.osgi.SbtOsgi.OsgiKeys._

organization := "org.scalawag.druthers"

name := "druthers"

version := "0.2-SNAPSHOT"

// When I put this at 2.10.0, the tests can't find the scala classes (ever since upgrading to sbt 0.13.0)
scalaVersion := "2.11.8"

scalacOptions ++= Seq("-unchecked","-deprecation","-feature","-target:jvm-1.6")

// Right now, the reflection stuff is not thread-safe so we have to execute our tests in sequence.
// See: http://docs.scala-lang.org/overviews/reflection/thread-safety.html
parallelExecution in Test := false

parallelExecution in jacoco.Config := false

resolvers += Resolver.sonatypeRepo("releases")

libraryDependencies ++= Seq(
  "org.scalawag.timber" %% "timber-api" % "0.6.0",
  "org.scalawag.timber" %% "timber-backend" % "0.6.0" % "test",
  "org.scalatest" %% "scalatest" % "3.0.0" % "test"
)

publishMavenStyle := true

publishArtifact in Test := false

publishTo := {
  val nexus = "https://oss.sonatype.org/"
  if (version.value.trim.endsWith("SNAPSHOT"))
    Some("snapshots" at nexus + "content/repositories/snapshots")
  else
    Some("releases"  at nexus + "service/local/staging/deploy/maven2")
}

pomIncludeRepository := { _ => false }

pomExtra :=
  <url>http://github.com/scalawag/druthers</url>
  <licenses>
    <license>
      <name>BSD-style</name>
      <url>http://www.opensource.org/licenses/bsd-license.php</url>
      <distribution>repo</distribution>
    </license>
  </licenses>
  <scm>
    <url>http://github.com/scalawag/druthers.git</url>
    <connection>scm:git:git://github.com/scalawag/druthers.git</connection>
  </scm>
  <developers>
    <developer>
      <id>justinp</id>
      <name>Justin Patterson</name>
      <email>justin@scalawag.org</email>
      <url>https://github.com/justinp</url>
    </developer>
  </developers>

seq(jacoco.settings : _*)

osgiSettings

exportPackage += "org.scalawag.druthers"

// druthers -- Copyright 2013 Justin Patterson -- All Rights Reserved
