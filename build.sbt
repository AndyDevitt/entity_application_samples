name := "entity_application_samples"

version := "0.1"

scalaVersion := "2.12.8"

resolvers ++= Seq(
  Resolver.sonatypeRepo("releases"),
  Resolver.sonatypeRepo("snapshots")
)

libraryDependencies +=
  "org.typelevel" %% "cats-core" % "1.0.0"

libraryDependencies +=
  "org.typelevel" %% "cats-effect" % "1.1.0"

libraryDependencies +=
  "com.chuusai" %% "shapeless" % "2.3.3"

scalacOptions ++= Seq(
  "-Xfatal-warnings",
  "-Ypartial-unification",
  "-feature",
  "-deprecation",
  //"-language:existentials",
  "-language:higherKinds",
  //"-language:implicitConversions"
)
