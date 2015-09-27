name := "scalaad"

version := "0.0.1"

scalaVersion := "2.11.7"

lazy val root = project.in(file("."))

libraryDependencies ++= Seq()

scalacOptions += "-feature"

resolvers ++= Seq(
  "Sonatype OSS Snapshots" at "https://oss.sonatype.org/content/repositories/snapshots",
  "Sonatype release Repository" at "http://oss.sonatype.org/service/local/staging/deploy/maven2/",
  "Local Maven Repository" at "file://"+Path.userHome.absolutePath+"/.m2/repository/"
)
