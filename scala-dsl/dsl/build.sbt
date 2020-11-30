
scalaVersion := "2.12.3"

name := "dsl"
organization := "ch.epfl.scala"
version := "1.0"

scalacOptions ++= Seq( // "-Xfatal-warnings",
  "-Ypartial-unification" )

libraryDependencies += "org.typelevel" %% "cats-core" % "1.0.0"
libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.2" % Test