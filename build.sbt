lazy val root = (project in file("."))
  .settings(
    name         := "ScalaChess",
    scalaVersion := "2.13.14",
    version      := "0.1.0",
    libraryDependencies   +=
      "com.whitehatgaming" % "UserInputFile" % "1.0" from "file:///Users/Utilizador/Documents/ChessProj/chess/libs/userinput.jar",
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
