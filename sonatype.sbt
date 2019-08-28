// Your profile name of the sonatype account. The default is the same with the organization value
sonatypeProfileName := "io.github.transcendent-ai-labs"

// To sync with Maven central, you need to supply the following information:
publishMavenStyle := true

// License of your choice
licenses := Seq("APL2" -> url("http://www.apache.org/licenses/LICENSE-2.0.txt"))

// Where is the source code hosted
//import xerial.sbt.Sonatype._
//sonatypeProjectHosting := Some(GitHubHosting("username", "projectName", "user@example.com"))
// or
//sonatypeProjectHosting := Some(GitLabHosting("username", "projectName", "user@example.com"))

// or if you want to set these fields manually
homepage := Some(url("https://transcendent-ai-labs.github.io/autograd/"))

scmInfo := Some(
  ScmInfo(
    url("https://github.com/transcendent-ai-labs/autograd"),
    "git@github.com:transcendent-ai-labs/autograd.git"
  )
)

developers := List(
  Developer(
    id="mandar2812",
    name="Mandar Chandorkar",
    email="mandar2812@gmail.com",
    url=url("http://mandar2812.github.io/"))
)