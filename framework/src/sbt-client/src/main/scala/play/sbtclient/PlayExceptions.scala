/*
 * Copyright (C) 2009-2013 Typesafe Inc. <http://www.typesafe.com>
 */
package play.sbtclient

import sbt.IO
import play.api._
import java.io.File

/**
 * A marker trait for a top-level exception handler to know that this exception
 * doesn't make sense to display.
 */
trait UnprintableException extends Throwable

/**
 * A marker trait that refines UnprintableException to indicate to a top-level exception handler
 * that the code throwing this exception has already provided feedback to the user about the error condition.
 */
trait FeedbackProvidedException extends UnprintableException

trait PlayExceptions {
  import scala.language.implicitConversions

  private implicit def convertToOption[T](o: xsbti.Maybe[T]): Option[T] =
    if (o.isDefined()) Some(o.get())
    else None

  def filterAnnoyingErrorMessages(message: String): String = {
    val overloaded = """(?s)overloaded method value (.*) with alternatives:(.*)cannot be applied to(.*)""".r
    message match {
      case overloaded(method, _, signature) => "Overloaded method value [" + method + "] cannot be applied to " + signature
      case msg => msg
    }
  }

  case class UnexpectedException(message: Option[String] = None, unexpected: Option[Throwable] = None) extends PlayException(
    "Unexpected exception",
    message.getOrElse {
      unexpected.map(t => "%s: %s".format(t.getClass.getSimpleName, t.getMessage)).getOrElse("")
    },
    unexpected.orNull
  )

  case class CompilationException(problem: xsbti.Problem) extends PlayException.ExceptionSource(
    "Compilation error", filterAnnoyingErrorMessages(problem.message)) {
    def line = problem.position.line.map(m => m.asInstanceOf[java.lang.Integer]).orNull
    def position = problem.position.pointer.map(m => m.asInstanceOf[java.lang.Integer]).orNull
    def input = problem.position.sourceFile.map(IO.read(_)).orNull
    def sourceName = problem.position.sourceFile.map(_.getAbsolutePath).orNull
  }

  case class TemplateCompilationException(source: File, message: String, atLine: Int, column: Int) extends PlayException.ExceptionSource(
    "Compilation error", message) with FeedbackProvidedException {
    def line = atLine
    def position = column
    def input = IO.read(source)
    def sourceName = source.getAbsolutePath
  }

  case class RoutesCompilationException(source: File, message: String, atLine: Option[Int], column: Option[Int]) extends PlayException.ExceptionSource(
    "Compilation error", message) with FeedbackProvidedException {
    def line = atLine.map(_.asInstanceOf[java.lang.Integer]).orNull
    def position = column.map(_.asInstanceOf[java.lang.Integer]).orNull
    def input = IO.read(source)
    def sourceName = source.getAbsolutePath
  }

  case class AssetCompilationException(source: Option[File], message: String, atLine: Option[Int], column: Option[Int]) extends PlayException.ExceptionSource(
    "Compilation error", message) with FeedbackProvidedException {
    def line = atLine.map(_.asInstanceOf[java.lang.Integer]).orNull
    def position = column.map(_.asInstanceOf[java.lang.Integer]).orNull
    def input = source.filter(_.exists()).map(IO.read(_)).orNull
    def sourceName = source.map(_.getAbsolutePath).orNull
  }

}

object PlayExceptions extends PlayExceptions
