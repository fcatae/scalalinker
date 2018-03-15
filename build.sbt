name := "scalalinker"

version := "0.0.1-SNAPSHOT"

scalaVersion := "2.12.4"

scalacOptions ++= Seq("-deprecation", "-feature")

libraryDependencies ++= Seq(
  "org.scala-lang.modules" %% "scala-parser-combinators" % "1.0.6",
  "junit" % "junit" % "4.10" % "test",
  "org.specs2" %% "specs2-core" % "4.0.2" % "test",
  "org.specs2" %% "specs2-junit" % "4.0.2" % "test",
  "org.scalaz" %% "scalaz" % "7.2.20",
  "com.github.nscala-time" %% "nscala-time" % "2.18.0"
)

resolvers ++= Seq("snapshots" at "http://oss.sonatype.org/content/repositories/snapshots",
                  "releases" at "http://oss.sonatype.org/content/repositories/releases")
