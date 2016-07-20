libraryDependencies ++= Seq(
  "org.scalanlp" %% "breeze"         % "0.12",
  "org.scalanlp" %% "breeze-natives" % "0.12"
)

initialCommands in console :=
"""
  |import breeze.linalg._
  |import scalaad.impl.breeze.Implicits._
""".stripMargin