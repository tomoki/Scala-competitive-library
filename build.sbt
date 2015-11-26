libraryDependencies += "org.scalatest" %% "scalatest" % "2.2.4" % "test"

scalaVersion := "2.11.7"

scalacOptions ++= Seq(
  "-deprecation",
  "-feature",
  "-unchecked",
  "-Xlint",
  "-Ywarn-dead-code",
  "-Ywarn-numeric-widen",
  "-Ywarn-unused",
  "-Ywarn-value-discard"
  // 警告をエラーにする（お好みに応じて）
  // , "-Xfatal-warnings"
)
