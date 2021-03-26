val scala3Version = "3.0.0-RC1"
useCoursier := false
lazy val root = project
  .in(file("."))
  .settings(
    name := "scala3-simple",
    version := "0.1.0",
//    scalacOption := Seq("-Yindent-colons"),  
 
    scalaVersion := scala3Version, 
//    libraryDependencies += "org.scala-lang.modules" %% "scala-collection-contrib" % "0.2.2",
    libraryDependencies += "com.novocode" % "junit-interface" % "0.11" % "test",
    libraryDependencies += "org.scala-lang" %% "scala3-staging" % scalaVersion.value,
//    libraryDependencies += "org.typelevel" %% "cats-core" % "2.3.0-M1"


)
