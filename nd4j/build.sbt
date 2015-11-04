libraryDependencies ++= Seq(
  "org.nd4j" % "nd4j"             % "0.4-rc3.8",
  "org.nd4j" % "nd4j-jcublas-7.5" % "0.4-rc3.8",
  "org.nd4j" %% "nd4s"            % "0.4-rc3.8"
)

initialCommands in console :=
"""
  |import com.kogecoo.scalaad.impl.nd4j.Implicits._
  |import org.nd4s.Implicits._
""".stripMargin