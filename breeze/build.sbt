libraryDependencies ++= Seq(
  "org.scalanlp" %% "breeze"         % "0.11.2",
  "org.scalanlp" %% "breeze-natives" % "0.11.2"
)

initialCommands in console :=
"""
  |import breeze.linalg._
  |import com.kogecoo.scalaad.breeze.BreezeRule.Implicits._
""".stripMargin