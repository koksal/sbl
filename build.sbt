scalaVersion := "2.10.3"

libraryDependencies += "org.scala-lang" % "scala-swing" % "2.10.3"

libraryDependencies += "org.scalatest" % "scalatest_2.11" % "2.1.4" % "test"

if(System.getProperty("os.name").indexOf("nux") >= 0) {
  unmanagedBase <<= baseDirectory { base => base / "lib" / "linux" }
} else if (System.getProperty("os.name").indexOf("Mac") >= 0) {
  unmanagedBase <<= baseDirectory { base => base / "lib" / "mac" }
} else {
  unmanagedBase <<= baseDirectory { base => base / "lib" }
}

fork := true

outputStrategy := Some(StdoutOutput)
