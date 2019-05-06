name := "GenericProgramming"

version := "0.1"

scalaVersion := "2.12.8"
//val ccompileOptions = Seq("-Yliteral-types","-Ypartial-unification","-language:higherKinds")
//scalacOptions ++= ccompileOptions

libraryDependencies ++= Seq("com.chuusai" %% "shapeless" % "2.3.3",
  "org.scala-lang" % "scala-reflect" % "2.12.8",
  "org.scala-lang" % "scala-compiler" % "2.12.8",
  "org.typelevel" %% "cats-core" % "1.6.0"
)