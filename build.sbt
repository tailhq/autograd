

lazy val root = project.in(file("."))
  .aggregate(scalaad, breeze, nd4j)
  .settings(commonSettings: _*)
  .settings(name := "scalaad")

lazy val scalaad = project.in(file("scalaad"))
  .settings(commonSettings:_*)

lazy val breeze = project.in(file("breeze"))
  .dependsOn(scalaad)
  .settings(commonSettings:_*)

lazy val nd4j = project.in(file("nd4j"))
  .dependsOn(scalaad)
  .settings(commonSettings:_*)

lazy val example = project.in(file("example"))
  .dependsOn(breeze)
  .dependsOn(nd4j)
  .dependsOn(scalaad)
  .settings(commonSettings:_*)

lazy val commonSettings = Seq(
  organization        :=  "com.kogecoo",
  name                <<= name("scalaad-" + _),
  version             :=  "0.0.1-SNAPSHOT",
  scalaVersion        :=  "2.11.7",
  scalacOptions       ++= commonScalacOptions,
  resolvers           ++= commonResolvers,
  libraryDependencies ++= commonLibraryDependencies
)

lazy val commonScalacOptions = Seq(
  "-feature",
  "-deprecation",
  "-unchecked",
  "-language:postfixOps",
  "-Xlint",
  "-Ywarn-dead-code"
)

lazy val commonResolvers = Seq(
  "Sonatype OSS Snapshots" at "https://oss.sonatype.org/content/repositories/snapshots",
  "Sonatype release Repository" at "http://oss.sonatype.org/service/local/staging/deploy/maven2/",
  "Local Maven Repository" at "file://"+Path.userHome.absolutePath+"/.m2/repository/"
)

lazy val commonLibraryDependencies = Seq(
  //"org.scalacheck" %% "scalacheck" % "1.25.5" % "test", //TODO: future
  "org.scalatest" %% "scalatest" % "2.2.4" % "test"
)

