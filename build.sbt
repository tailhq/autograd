lazy val commonSettings = Seq(
  organization := "io.github.transcendent-ai-labs",
  name ~= (n => "autograd-" + n),
  version := "0.0.1-SNAPSHOT",
  scalaVersion := "2.12.8",
  crossScalaVersions := Seq("2.11.8"),
  scalacOptions ++= commonScalacOptions,
  resolvers ++= commonResolvers,
  libraryDependencies ++= commonLibraryDependencies
)

lazy val commonPublishSettings = Seq(
  licenses := Seq(
    "Apache License Version 2.0" -> url(
      "http://www.apache.org/licenses/LICENSE-2.0"
    )
  ),
  homepage := Some(url("http://kogecoo.github.com/scalaad")),
  publishMavenStyle := true,
  publishArtifact in Test := false,
  pomIncludeRepository := (_ => false),
  pomExtra := commonPomExtra //,
  //publishTo               ~= version {(v: String) => choosePublishTo(v)}
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
  "Local Maven Repository" at "file://" + Path.userHome.absolutePath + "/.m2/repository/"
)

lazy val commonLibraryDependencies = Seq(
  "org.scalacheck" %% "scalacheck" % "1.12.6" % "test",
  "org.scalatest"  %% "scalatest"  % "3.0.0"  % "test"
)

lazy val commonPomExtra = {
  <scm>
    <url>git@github.com:kogecoo/scalaad.git</url>
    <connection>scm:git:git@github.com:kogecoo/scalaad.git</connection>
  </scm>
  <developers>
    <developer>
      <id>kogecoo</id>
      <name>kogecoo</name>
      <url>http://kogecoo.github.com</url>
    </developer>
  </developers>
}

lazy val testSettings = Seq(
  testOptions += Tests.Argument(TestFrameworks.ScalaCheck, "-verbosity", "2"),
  testOptions += Tests.Argument(TestFrameworks.ScalaTest, "-oDF")
)

def choosePublishTo(v: String) = {
  if (v.trim.endsWith("SNAPSHOT"))
    Some(
      Resolver.file("file", new File(Path.userHome.absolutePath + "/temporary"))
    )
  else
    Some(
      Resolver.file("file", new File(Path.userHome.absolutePath + "/temporary"))
    )
}

lazy val core = project
  .in(file("autograd-core"))
  .settings(commonSettings)
  .settings(name := "autograd-core")

lazy val breeze = project
  .in(file("breeze"))
  .dependsOn(core)
  .settings(commonSettings)

/* lazy val nd4j = project.in(file("nd4j"))
  .dependsOn(core)
  .settings(commonSettings)
  .settings(scalaVersion := "2.11.8") */

lazy val examples = project
  .in(file("examples"))
  .dependsOn(breeze)
  .dependsOn(core)
  .settings(commonSettings: _*)


lazy val root = project
  .in(file("."))
  .dependsOn(core, breeze, examples)
  .aggregate(core, breeze, examples)
  .settings(commonSettings: _*)
  .settings(name := "autograd")
  .settings(publish := {})
  .settings(testSettings: _*)

