lazy val root = (project in file(".")).
  settings(
    inThisBuild(List(
      organization := "com.example",
      scalaVersion := "2.13.3"
    )),
    name := "scalatest-example"
  )

scalacOptions := Seq("-deprecation")

libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.7" % Test
libraryDependencies += "org.scalamock" %% "scalamock" % "5.1.0" % Test
