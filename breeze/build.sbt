libraryDependencies ++= Seq(
  "org.scalanlp" %% "breeze"         % "1.0" % "compile",
  "org.scalanlp" %% "breeze-natives" % "1.0" % "compile"
)

initialCommands in console :=
"""
  |import breeze.linalg._
  |import com.kogecoo.scalaad.breeze.BreezeRule.Implicits._
""".stripMargin