libraryDependencies ++= Seq(
  "org.nd4j" %  "nd4j-api"   % "0.4-rc3",
  "org.nd4j" %  "nd4j-jblas" % "0.4-rc3",
  "org.nd4j" %% "nd4s"       % "0.4-rc3",
  "org.scalanlp" %% "breeze"         % "0.11.2",
  "org.scalanlp" %% "breeze-natives" % "0.11.2"
)

initialCommands in console := """ """