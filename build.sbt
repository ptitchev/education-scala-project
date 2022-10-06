val scala3Version = "3.2.0"

lazy val root = project
  .in(file("."))
  .settings(
    name := "My beautiful project", // TODO: name your project
    version := "0.1.0-SNAPSHOT",
    developers := List( // TODO: replace the following developer by your team developers
      Developer(
        id    = "shayanA",
        name  = "Shayan Arnal",
        email = "shayan.arnal@edu.esiee.fr",
        url   = url("https://github.com/ptitchev/education-scala-project/tree/shayanA")
      )
    ),
    scalaVersion := scala3Version,
    libraryDependencies ++= Seq("org.scalameta" %% "munit" % "0.7.29" % Test)
  )
