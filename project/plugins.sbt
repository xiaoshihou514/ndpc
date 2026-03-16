resolvers += Resolver.mavenCentral
resolvers += Resolver.sonatypeRepo("releases")
resolvers += Resolver.url(
  "sbt-plugin-releases",
  url("https://repo.scala-sbt.org/scalasbt/sbt-plugin-releases/")
)(Resolver.ivyStylePatterns)
resolvers += Resolver.url(
  "typesafe-ivy-releases",
  url("https://repo.typesafe.com/typesafe/ivy-releases/")
)(Resolver.ivyStylePatterns)

addSbtPlugin("org.scala-js" % "sbt-scalajs" % "1.18.2")
addSbtPlugin("com.eed3si9n" % "sbt-assembly" % "2.3.1")
addSbtPlugin("org.scala-native" % "sbt-scala-native" % "0.5.9")
addSbtPlugin("org.scalameta" % "sbt-scalafmt" % "2.5.2")
