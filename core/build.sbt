name := "tiramisu-core"

version := "0.1-SNAPSHOT"

organization := "com.github.igor-petruk.tiramisu"

scalaVersion := "2.10.0-RC1"

autoScalaLibrary := true

crossScalaVersions := Seq("2.10.0-RC1")

/** Dependencies */
resolvers ++= Seq(
	"snapshots-repo" at "https://oss.sonatype.org/content/repositories/snapshots/",
	"jars-repo" at "https://oss.sonatype.org/content/groups/scala-tools/"
)

libraryDependencies <<= scalaVersion { scala_version => Seq(
	"org.scala-lang" % "scala-library" % "2.10.0-RC1",
  "javax.servlet" % "servlet-api" % "3.0-alpha-1", 
  "org.codehaus.jackson" % "jackson-mapper-lgpl" % "1.9.7", 
  "org.apache.commons" % "commons-jexl" % "2.1.1",
   "commons-io" % "commons-io" % "2.4",
  "org.scalatest" % "scalatest" % "1.4.RC2" % "test", 
  "junit" % "junit" % "4.10" % "test" 
  )
}

/** Compilation */
javacOptions ++= Seq()

javaOptions += "-Xmx2G"

scalacOptions ++= Seq("-deprecation", "-unchecked")

maxErrors := 20 

pollInterval := 1000

logBuffered := false

cancelable := true


