organization := "org.scalawag.druthers"

name := "druthers"

version := "0.1-SNAPSHOT"

scalaVersion := "2.10.2"

scalacOptions ++= Seq("-unchecked","-deprecation","-feature")

resolvers += "sonatype-oss-snapshots" at "http://oss.sonatype.org/content/repositories/snapshots/"

libraryDependencies ++= Seq(
  "org.scalawag.timber" % "timber-api" % "0.3-SNAPSHOT",
  "org.scala-lang" % "scala-reflect" % "2.10.2",
  "org.scalawag.timber" % "timber" % "0.3-SNAPSHOT" % "test",
  "org.mockito" % "mockito-all" % "1.9.5" % "test",
  "org.scalatest" %% "scalatest" % "1.9.1" % "test"
)

// druthers -- Copyright 2013 Justin Patterson -- All Rights Reserved
