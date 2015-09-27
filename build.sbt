enablePlugins(ScalaJSPlugin)

workbenchSettings

organization := "com.github.rhyskeepence"
name := "sjs-react-select"

version := "0.1.0"

scalaVersion := "2.11.7"

jsDependencies += RuntimeDOM

requiresDOM := true

bootSnippet := "rhyskeepence.example.Example().main()"

refreshBrowsers <<= refreshBrowsers.triggeredBy(fastOptJS in Compile)

libraryDependencies += "com.github.japgolly.scalajs-react" %%% "core" % "0.9.2"
jsDependencies += "org.webjars" % "react" % "0.13.3" / "react-with-addons.js" minified "react-with-addons.min.js" commonJSName "React"

lazy val root = project.in(file("."))

