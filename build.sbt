import de.johoop.jacoco4sbt._
import JacocoPlugin._
import com.typesafe.sbt.osgi.SbtOsgi.OsgiKeys._

organization := "org.scalawag.druthers"

name := "druthers"

version := "0.1-SNAPSHOT"

// When I put this at 2.10.0, the tests can't find the scala classes (ever since upgrading to sbt 0.13.0)
scalaVersion := "2.10.2"

scalacOptions ++= Seq("-unchecked","-deprecation","-feature","-target:jvm-1.6")

// Right now, the reflection stuff is not thread-safe so we have to execute our tests in sequence.
// See: http://docs.scala-lang.org/overviews/reflection/thread-safety.html
parallelExecution in Test := false

parallelExecution in jacoco.Config := false

resolvers += "sonatype-oss-snapshots" at "http://oss.sonatype.org/content/repositories/snapshots/"

libraryDependencies ++= Seq(
  "org.scalawag.timber" % "timber-api" % "0.3-SNAPSHOT" changing,
  "org.scala-lang" % "scala-reflect" % "2.10.0",
  "org.scalawag.timber" % "timber" % "0.3-SNAPSHOT" % "test" changing,
  "org.mockito" % "mockito-all" % "1.9.5" % "test",
  "org.scalatest" %% "scalatest" % "1.9.1" % "test"
)

publishMavenStyle := true

publishArtifact in Test := false

publishTo <<= version { (v: String) =>
    val nexus = "https://oss.sonatype.org/"
    if (v.trim.endsWith("SNAPSHOT"))
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
