/*
 * Copyright (C) 2009-2013 Typesafe Inc. <http://www.typesafe.com>
 */
package play

import sbt._
import sbt.Keys._
import play.runsupport.protocol.PlayForkSupportResult

trait PlayInternalKeys {
  type ClassLoaderCreator = (String, Array[URL], ClassLoader) => ClassLoader

  /**
   * Configuration for the Play fork-runner dependencies.
   */
  val ForkRunner = config("fork-runner").hide

  val playNotifyServerStart = inputKey[Unit]("Sends an event when the forked dev-server has started")
  val playFullBackgroundRunTaskBuilder = TaskKey[(ScopedKey[_], BackgroundJobService, File, File, ProjectRef, Seq[String], Classpath, Classpath, Seq[String], File, Classpath, Int, Int, String, Seq[String]) => BackgroundJobHandle]("play-full-background-run-task-builder")
  val playBackgroundRunTaskBuilderWithClasspaths = TaskKey[(Seq[String],Classpath,Classpath,String) => BackgroundJobHandle]("play-background-run-task-builder-with-classpaths")
  val playBackgroundRunTaskBuilder = TaskKey[Seq[String] => BackgroundJobHandle]("play-background-run-task-builder")
  val playForkedRunnerBootstrapDependencies = SettingKey[Seq[ModuleID]]("play-forked-runner-bootstrap-dependencies")
  val playForkedRunnerBootstrapClasspath = TaskKey[Classpath]("play-forked-runner-bootstrap-classpath")
  val playForkedRunnerTaskName = TaskKey[String]("play-forked-runner-task-name")
  val playDependencyClasspath = TaskKey[Classpath]("play-dependency-classpath")
  val playReloaderClasspath = TaskKey[Classpath]("play-reloader-classpath")
  val playCommonClassloader = TaskKey[ClassLoader]("play-common-classloader")
  val playDependencyClassLoader = TaskKey[ClassLoaderCreator]("play-dependency-classloader")
  val playReloaderClassLoader = TaskKey[ClassLoaderCreator]("play-reloader-classloader")
  val playReload = TaskKey[sbt.inc.Analysis]("play-reload")
  val buildRequire = TaskKey[Seq[(File, File)]]("play-build-require-assets")
  val playCompileEverything = TaskKey[Seq[sbt.inc.Analysis]]("play-compile-everything")
  val playAssetsWithCompilation = TaskKey[sbt.inc.Analysis]("play-assets-with-compilation")

  val playStop = TaskKey[Unit]("play-stop", "Stop Play, if it has been started in non blocking mode")

  val playAllAssets = TaskKey[Seq[(String, File)]]("play-all-assets")
  val playPrefixAndAssets = TaskKey[(String, File)]("play-prefix-and-assets")
  val playPrefixAndPipeline = TaskKey[(String, Seq[(File, String)])]("play-prefix-and-pipeline")
  val playAssetsClassLoader = TaskKey[ClassLoader => ClassLoader]("play-assets-classloader")
  val playPackageAssetsMappings = TaskKey[Seq[(File, String)]]("play-package-assets-mappings")

  val playDefaultForkRunSupport = TaskKey[PlayForkSupportResult]("play-default-fork-run-support")

  @deprecated(message = "Use PlayKeys.playMonitoredFiles instead", since = "2.3.2")
  val playMonitoredFiles = PlayImport.PlayKeys.playMonitoredFiles
}
