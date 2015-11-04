libraryDependencies ++= Seq(
  "org.nd4j" % "nd4j-api"   % "0.4-rc3",
  "org.nd4j" % "nd4j-jblas" % "0.4-rc3",
  "org.nd4j" %% "nd4s"      % "0.4-rc3"
)

initialCommands in console :=
"""
  |import com.kogecoo.scalaad.impl.nd4j.Implicits._
  |import org.nd4s.Implicits._
""".stripMargin