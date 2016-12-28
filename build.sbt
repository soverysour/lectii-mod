import scalariform.formatter.preferences._
import com.typesafe.sbt.SbtScalariform
import com.typesafe.sbt.SbtScalariform.ScalariformKeys

name := "Romana"
organization := "Draconis"
version := "0.1.0"
scalaVersion := "2.12.0"
resolvers ++= Seq(
	"Sonatype OSS Snapshots" at "http://oss.sonatype.org/content/repositories/snapshots/*"
)

libraryDependencies ++= Seq(
	"org.scalatest" %% "scalatest" % "3.0.0" % "test",
	"org.scalacheck" %% "scalacheck" % "1.13.4" % "test"
)

scalacOptions ++= Seq(
	"-target:jvm-1.8",
	"-deprecation",
	"-encoding", "UTF-8",
	"-feature",
	"-language:existentials",
	"-language:higherKinds",
	"-language:implicitConversions",
	"-language:experimental.macros",
	"-unchecked",
	"-Ywarn-unused-import",
	"-Ywarn-nullary-unit",
	"-Xfatal-warnings",
	"-Xlint",
	//"-Yinline-warnings",
	"-Ywarn-dead-code",
	"-Xfuture"
)

initialCommands := "import Draconis.romana._"

SbtScalariform.scalariformSettings

ScalariformKeys.preferences := ScalariformKeys.preferences.value
	.setPreference(AlignSingleLineCaseStatements, true)
	.setPreference(DoubleIndentClassDeclaration, true)
	.setPreference(RewriteArrowSymbols, false)
