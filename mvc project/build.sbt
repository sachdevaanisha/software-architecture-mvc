ThisBuild / scalaVersion := "2.13.8"

ThisBuild / version := "1.0-SNAPSHOT"

lazy val root = (project in file("."))
  .enablePlugins(PlayScala)
  .settings(
    name := """multipage_approach""",
/*
    libraryDependencies ++= Seq(
      guice,
      "org.scalatestplus.play" %% "scalatestplus-play" % "5.1.0" % Test,
      "com.adrianhurt" %% "play-bootstrap" % "1.6.1-P28-B3"
    )
*/
    libraryDependencies ++= Seq( jdbc , ehcache , ws , specs2 % Test , guice ,
  "com.adrianhurt" %% "play-bootstrap" % "1.6.1-P28-B3"
)



)