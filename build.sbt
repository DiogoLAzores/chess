lazy val root = (project in file("."))
  .settings(
    name         := "ScalaChess",
    scalaVersion := "2.13.14",
    version      := "0.2.0",
    libraryDependencies   +=
      "com.whitehatgaming" % "UserInputFile" % "1.0" from "file:///[ADD PATH TO userinput.jar HERE]",
    scalacOptions ++= Seq(
      "-deprecation",
      "-encoding",
      "UTF-8",
      "-language:higherKinds",
      "-language:postfixOps",
      "-feature",
      "-Ymacro-annotations"
    )
  )
