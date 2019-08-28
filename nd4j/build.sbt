libraryDependencies ++= Seq(
  "org.nd4j" % "nd4j-native-platform" % "0.9.1",
  "org.nd4j" %% "nd4s" % "0.9.1"
)

initialCommands in console :=
"""
  |import com.kogecoo.scalaad.nd4j.Nd4jRule.Implicits._
  |import org.nd4s.Implicits._
""".stripMargin