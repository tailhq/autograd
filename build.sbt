name := "scalaad"

version := "0.0.1"

scalaVersion := "2.11.7"

lazy val root = project.in(file("."))

libraryDependencies ++= Seq(
  "org.nd4j" %% "nd4s" % "0.4-rc3",
  "org.nd4j" % "nd4j-api" % "0.4-rc3",
  "org.nd4j" % "nd4j-jblas" % "0.4-rc3",
  "org.scalanlp" %% "breeze" % "0.11.2",
  "org.scalanlp" %% "breeze-natives" % "0.11.2"
)

scalacOptions += "-feature"

resolvers ++= Seq(
  "Sonatype OSS Snapshots" at "https://oss.sonatype.org/content/repositories/snapshots",
  "Sonatype release Repository" at "http://oss.sonatype.org/service/local/staging/deploy/maven2/",
  "Local Maven Repository" at "file://"+Path.userHome.absolutePath+"/.m2/repository/"
)
