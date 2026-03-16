import sbt._
import Keys._
import scala.sys.process._
import sbtassembly.AssemblyPlugin.autoImport._
import org.scalajs.sbtplugin.ScalaJSPlugin
import org.scalajs.sbtplugin.ScalaJSPlugin.autoImport._

ThisBuild / organization := "io.github.xiaoshihou514"
ThisBuild / version := "0.1.0-SNAPSHOT"
ThisBuild / scalaVersion := "3.6.0"
ThisBuild / scalacOptions ++= Seq(
  "-feature",
  "-Yexplicit-nulls"
)

lazy val catsEffectVersion = "3.7.0"

// Quick commands:
//   sbt test
//   sbt coreJVM/test
//   sbt cli/test
//   sbt web/fastLinkJS
//   sbt "cli/run -- check example.ndp"
//   sbt cli/assembly
//   sbt cli/graalNativeImage
//   sbt cliNative/rootNativeLink

lazy val graalNativeImage = taskKey[File]("Build a native-image binary from the cli assembly jar")
lazy val rootNativeLink = taskKey[File]("Build the Scala Native binary and copy it to the repository root")

lazy val commonResolvers = Seq(Resolver.mavenCentral)

lazy val coreJVM = (project in file("core/jvm"))
  .settings(
    resolvers ++= commonResolvers,
    name := "ndpc-core-jvm",
    libraryDependencies ++= Seq(
      "com.github.j-mie6" %% "parsley" % "5.0.0-M16",
      "com.lihaoyi" %% "os-lib" % "0.11.8" % Test,
      "org.scalatest" %% "scalatest" % "3.2.19" % Test
    ),
    Compile / unmanagedSourceDirectories += baseDirectory.value.getParentFile / "shared" / "src" / "main" / "scala",
    Test / fork := true,
    Test / javaOptions += s"-Dndpc.repoRoot=${(LocalRootProject / baseDirectory).value.getAbsolutePath}"
  )

lazy val coreJS = (project in file("core/js"))
  .enablePlugins(ScalaJSPlugin)
  .settings(
    resolvers ++= commonResolvers,
    name := "ndpc-core-js",
    scalaJSUseMainModuleInitializer := false,
    libraryDependencies ++= Seq(
      "com.github.j-mie6" %%% "parsley" % "5.0.0-M16"
    ),
    Compile / unmanagedSourceDirectories += baseDirectory.value.getParentFile / "shared" / "src" / "main" / "scala"
  )

lazy val core = (project in file("core"))
  .aggregate(coreJVM, coreJS)
  .settings(
    name := "ndpc-core",
    publish / skip := true
  )

lazy val cli = (project in file("cli"))
  .dependsOn(coreJVM)
  .settings(
    resolvers ++= commonResolvers,
    name := "ndpc-cli",
    libraryDependencies ++= Seq(
      "com.monovore" %% "decline" % "2.5.0",
      "org.typelevel" %% "cats-effect" % catsEffectVersion,
      "com.lihaoyi" %% "os-lib" % "0.11.8",
      "org.scalatest" %% "scalatest" % "3.2.19" % Test
    ),
    Compile / mainClass := Some("ndpc.Main"),
    assembly / mainClass := Some("ndpc.Main"),
    assembly / assemblyOutputPath := (LocalRootProject / baseDirectory).value / "ndpc.jar",
    assembly / assemblyJarName := "ndpc.jar",
    Test / fork := true,
    Test / javaOptions += s"-Dndpc.repoRoot=${(LocalRootProject / baseDirectory).value.getAbsolutePath}",
    graalNativeImage := {
      val log = streams.value.log
      val jar = (Compile / assembly).value
      val output = (LocalRootProject / baseDirectory).value / "ndpc-graal"
      val cmd = Seq("native-image", "-jar", jar.getAbsolutePath, output.getAbsolutePath)
      log.info(cmd.mkString(" "))
      val exit = Process(cmd, baseDirectory.value).!
      if (exit != 0) sys.error("native-image failed")
      output
    }
  )

// Optional Scala Native release path.
// This recompiles the shared/core CLI sources natively to avoid mixing JVM TASTy with
// a different Scala patch version in the native build.
lazy val cliNative = (project in file("cli-native"))
  .enablePlugins(scala.scalanative.sbtplugin.ScalaNativePlugin)
  .settings(
    resolvers ++= commonResolvers,
    name := "ndpc-cli-native",
    scalaVersion := "3.6.4",
    libraryDependencies ++= Seq(
      "com.github.j-mie6" %%% "parsley" % "5.0.0-M16",
      "com.monovore" %%% "decline" % "2.5.0",
      "org.typelevel" %%% "cats-effect" % catsEffectVersion,
      "com.lihaoyi" %%% "os-lib" % "0.11.6"
    ),
    Compile / unmanagedSourceDirectories ++= Seq(
      baseDirectory.value.getParentFile / "core" / "shared" / "src" / "main" / "scala",
      (cli / Compile / scalaSource).value
    ),
    Compile / mainClass := Some("ndpc.Main"),
    rootNativeLink := {
      val log = streams.value.log
      val linked = (Compile / nativeLink).value
      val output = (LocalRootProject / baseDirectory).value / "ndpc-native"
      IO.copyFile(linked, output, preserveLastModified = true)
      output.setExecutable(true, false)
      log.info(s"Copied: ${output.getAbsolutePath}")
      output
    },
    scala.scalanative.sbtplugin.ScalaNativePlugin.autoImport.nativeConfig ~=
      (_.withMode(scala.scalanative.build.Mode.releaseFast).withBaseName("ndpc-native"))
  )

lazy val web = (project in file("web"))
  .enablePlugins(org.scalajs.sbtplugin.ScalaJSPlugin)
  .dependsOn(coreJS)
  .settings(
    resolvers ++= commonResolvers,
    name := "ndpc-web",
    scalaJSUseMainModuleInitializer := true,
    libraryDependencies ++= Seq(
      "com.raquo" %%% "laminar" % "17.2.0"
    )
  )

lazy val root = (project in file("."))
  .aggregate(coreJVM, coreJS, cli, web)
  .settings(
    name := "ndpc",
    publish / skip := true
  )
