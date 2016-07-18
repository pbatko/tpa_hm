name := "ala"

version := "1.0"

//scalaVersion := "2.10.4"
scalaVersion := "2.11.8"
//scalaVersion := "2.11.1"

//ideaExcludeFolders += ".idea"

//ideaExcludeFolders += ".idea_modules"

testOptions in Test += Tests.Argument("-oS")

//libraryDependencies += "org.scalamock" % "scalamock-scalatest-support_2.10" % "3.0.1" % "test"


libraryDependencies += "org.scalactic" %% "scalactic" % "2.2.6"

libraryDependencies += "org.scalatest" %% "scalatest" % "2.2.6" % "test"

//libraryDependencies ++= Seq(
//  "com.typesafe.akka" %% "akka-actor"   % "2.2-M3",
//  "com.typesafe.akka" %% "akka-slf4j"   % "2.2-M3",
//  "com.typesafe.akka" %% "akka-remote"  % "2.2-M3",
//  "com.typesafe.akka" %% "akka-agent"   % "2.2-M3",
//  "com.typesafe.akka" %% "akka-testkit" % "2.2-M3" % "test"
//)

scalacOptions ++= List(
  "-unchecked",
  "-deprecation",
  "-Xlint",
  "-language:_",
  "-target:jvm-1.7",
  "-encoding", "UTF-8"
)

//javaOptions in run += "-Djava.library.path=../native"

fork := true