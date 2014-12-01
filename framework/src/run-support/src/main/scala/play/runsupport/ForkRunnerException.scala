/*
 * Copyright (C) 2009-2013 Typesafe Inc. <http://www.typesafe.com>
 */
package play.runsupport

import sbt.protocol.Problem
import sbt.IO

sealed abstract class ForkRunnerException[T <: Throwable](title: String,
                                                          message: String,
                                                          cause: T) extends Exception(s"$title - $message",cause) {
  def category: String
  def severity: xsbti.Severity
}

case class PlayExceptionNoSource[T <: Throwable](title: String,
                                                 message: String,
                                                 category: String,
                                                 severity: xsbti.Severity,
                                                 cause: T) extends ForkRunnerException[T](title,
                                                                                          message,
                                                                                          cause)

case class PlayExceptionWithSource[T <: Throwable](title: String,
                                                   message: String,
                                                   category: String,
                                                   severity: xsbti.Severity,
                                                   cause: T,
                                                   line: Int,
                                                   column: Int,
                                                   sourceFile: java.io.File) extends ForkRunnerException[T](title,
                                                                                                            message,
                                                                                                            cause) {
  def input = IO.read(sourceFile)
  def sourceName = sourceFile.getAbsolutePath
}
