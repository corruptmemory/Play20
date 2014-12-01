package play.runsupport

import play.api.libs.json._
import play.api.libs.json.Reads._
import play.api.libs.functional.syntax._
import play.api.data.validation.ValidationError
import play.runsupport.protocol._
import PlayExceptions._
import sbt.GenericSerializers._
import sbt.protocol._
import play.api.PlayException

trait LowPrioritySerializers { this:Serializers.type =>
  implicit val playExceptionWrites: Writes[PlayException] = new Writes[PlayException] {
    def writes(in:PlayException):JsValue = in match {
      case x: UnexpectedException => unexpectedExceptionWrites.writes(x)
      case x: CompilationException => compilationExceptionWrites.writes(x)
      case x: TemplateCompilationException => templateCompilationExceptionWrites.writes(x)
      case x: RoutesCompilationException => routesCompilationExceptionWrites.writes(x)
      case x: AssetCompilationException => assetCompilationExceptionWrites.writes(x)
    }
  }

  implicit val playExceptionReads: Reads[PlayException] = new Reads[PlayException] {
    def reads(in:JsValue):JsResult[PlayException] =
      unexpectedExceptionReads.reads(in) orElse
      compilationExceptionReads.reads(in) orElse
      templateCompilationExceptionReads.reads(in) orElse
      routesCompilationExceptionReads.reads(in) orElse
      assetCompilationExceptionReads.reads(in)
  }

  implicit val playExceptionFormat:Format[PlayException] = Format[PlayException](playExceptionReads,playExceptionWrites)
}

object Serializers extends LowPrioritySerializers {

  implicit def tuple2Reads[A, B](implicit aReads: Reads[A], bReads: Reads[B]): Reads[(A, B)] = Reads[(A, B)] { i =>
    i.validate[JsArray].flatMap { arr =>
      val s = aReads.reads(arr(0))
      val f = bReads.reads(arr(1))
      (s, f) match {
        case (JsSuccess(a, _), JsSuccess(b, _)) => JsSuccess((a, b))
        case (a @ JsError(_), JsSuccess(_, _)) => a
        case (JsSuccess(_, _), b @ JsError(_)) => b
        case (a @ JsError(_), b @ JsError(_)) => a ++ b
      }
    }
  }

  implicit def tuple2Writes[A, B](implicit aWrites: Writes[A], bWrites: Writes[B]): Writes[(A, B)] =
    Writes[(A, B)] { case (s, f) => JsArray(Seq(aWrites.writes(s), bWrites.writes(f))) }

  sealed trait LocalRegisteredFormat {
    type T
    def manifest: Manifest[T]
    def format: Format[T]
  }
  object LocalRegisteredFormat {
    def fromFormat[U](f: Format[U])(implicit mf: Manifest[U]): LocalRegisteredFormat =
      new LocalRegisteredFormat {
        type T = U
        val manifest = mf
        val format = f
      }
  }

  private implicit val throwableReads = sbt.GenericSerializers.throwableReads
  private implicit val throwableWrites = sbt.GenericSerializers.throwableWrites

  implicit val sourceMapTargetWrites: Writes[SourceMapTarget] = Json.writes[SourceMapTarget]
  implicit val sourceMapTargetReads: Reads[SourceMapTarget] = Json.reads[SourceMapTarget]
  implicit val sourceMapTargetFormat: Format[SourceMapTarget] = Format[SourceMapTarget](sourceMapTargetReads, sourceMapTargetWrites)

  implicit val sourceMapWrites:Writes[Map[String,SourceMapTarget]] = Writes.mapWrites[SourceMapTarget]
  implicit val sourceMapReads:Reads[Map[String,SourceMapTarget]] = Reads.mapReads[SourceMapTarget]
  implicit val sourceMapformat:Format[Map[String,SourceMapTarget]] = Format[Map[String,SourceMapTarget]](sourceMapReads, sourceMapWrites)

  implicit val playForkSupportResultWrites: Writes[PlayForkSupportResult] = Json.writes[PlayForkSupportResult]
  implicit val playForkSupportResultReads: Reads[PlayForkSupportResult] = Json.reads[PlayForkSupportResult]
  implicit val playForkSupportResultFormat: Format[PlayForkSupportResult] = Format[PlayForkSupportResult](playForkSupportResultReads, playForkSupportResultWrites)

  implicit val unexpectedExceptionReads: Reads[UnexpectedException] = Json.reads[UnexpectedException]
  implicit val unexpectedExceptionWrites: Writes[UnexpectedException] = Json.writes[UnexpectedException]
  implicit val unexpectedExceptionFormat: Format[UnexpectedException] = Format[UnexpectedException](unexpectedExceptionReads, unexpectedExceptionWrites)

  implicit val compilationExceptionReads: Reads[CompilationException] = Json.reads[CompilationException]
  implicit val compilationExceptionWrites: Writes[CompilationException] = Json.writes[CompilationException]
  implicit val compilationExceptionFormat: Format[CompilationException] = Format[CompilationException](compilationExceptionReads, compilationExceptionWrites)

  implicit val templateCompilationExceptionReads: Reads[TemplateCompilationException] = Json.reads[TemplateCompilationException]
  implicit val templateCompilationExceptionWrites: Writes[TemplateCompilationException] = Json.writes[TemplateCompilationException]
  implicit val templateCompilationExceptionFormat: Format[TemplateCompilationException] = Format[TemplateCompilationException](templateCompilationExceptionReads, templateCompilationExceptionWrites)

  implicit val routesCompilationExceptionReads: Reads[RoutesCompilationException] = Json.reads[RoutesCompilationException]
  implicit val routesCompilationExceptionWrites: Writes[RoutesCompilationException] = Json.writes[RoutesCompilationException]
  implicit val routesCompilationExceptionFormat: Format[RoutesCompilationException] = Format[RoutesCompilationException](routesCompilationExceptionReads, routesCompilationExceptionWrites)

  implicit val assetCompilationExceptionReads: Reads[AssetCompilationException] = Json.reads[AssetCompilationException]
  implicit val assetCompilationExceptionWrites: Writes[AssetCompilationException] = Json.writes[AssetCompilationException]
  implicit val assetCompilationExceptionFormat: Format[AssetCompilationException] = Format[AssetCompilationException](assetCompilationExceptionReads, assetCompilationExceptionWrites)

  def playExceptionNoSourceReads[T <: Throwable](implicit reads:Reads[T]): Reads[PlayExceptionNoSource[T]] =
    ((JsPath \ "title").read[String] and
     (JsPath \ "message").read[String] and
     (JsPath \ "category").read[String] and
     (JsPath \ "severity").read[xsbti.Severity] and
     (JsPath \ "cause").read[T])(PlayExceptionNoSource.apply[T] _)

  def playExceptionNoSourceWrites[T <: Throwable](implicit writes:Writes[T]): Writes[PlayExceptionNoSource[T]] =
    ((JsPath \ "title").write[String] and
     (JsPath \ "message").write[String] and
     (JsPath \ "category").write[String] and
     (JsPath \ "severity").write[xsbti.Severity] and
     (JsPath \ "cause").write[T])(unlift(PlayExceptionNoSource.unapply[T]))

  def playExceptionNoSourceFormat[T <: Throwable](implicit reads:Reads[T], writes:Writes[T]): Format[PlayExceptionNoSource[T]] = Format[PlayExceptionNoSource[T]](playExceptionNoSourceReads, playExceptionNoSourceWrites)

  def playExceptionWithSourceReads[T <: Throwable](implicit reads:Reads[T]): Reads[PlayExceptionWithSource[T]] =
    ((JsPath \ "title").read[String] and
     (JsPath \ "message").read[String] and
     (JsPath \ "category").read[String] and
     (JsPath \ "severity").read[xsbti.Severity] and
     (JsPath \ "cause").read[T] and
     (JsPath \ "line").read[Int] and
     (JsPath \ "column").read[Int] and
     (JsPath \ "sourceFile").read[java.io.File])(PlayExceptionWithSource.apply[T] _)

  def playExceptionWithSourceWrites[T <: Throwable](implicit writes:Writes[T]): Writes[PlayExceptionWithSource[T]] =
    ((JsPath \ "title").write[String] and
     (JsPath \ "message").write[String] and
     (JsPath \ "category").write[String] and
     (JsPath \ "severity").write[xsbti.Severity] and
     (JsPath \ "cause").write[T] and
     (JsPath \ "line").write[Int] and
     (JsPath \ "column").write[Int] and
     (JsPath \ "sourceFile").write[java.io.File])(unlift(PlayExceptionWithSource.unapply[T]))

  def playExceptionWithSourceFormat[T <: Throwable](implicit reads:Reads[T], writes:Writes[T]): Format[PlayExceptionWithSource[T]] = Format[PlayExceptionWithSource[T]](playExceptionWithSourceReads, playExceptionWithSourceWrites)

  implicit def forkRunnerExceptionReads[T <: Throwable](implicit rds:Reads[T]): Reads[ForkRunnerException[T]] = new Reads[ForkRunnerException[T]] {
    def reads(in:JsValue):JsResult[ForkRunnerException[T]] = ((in \ "type"),(in \ "value")) match {
      case (JsString("no-source"),v:JsValue) => playExceptionNoSourceReads[T](rds).reads(v)
      case (JsString("with-source"),v:JsValue) => playExceptionWithSourceReads[T](rds).reads(v)
      case _ => JsError("Cannot deserialize a ForkRunnerException: missing 'type' descriminator with a value of either 'no-source' or 'with-source'")
    }
  }
  implicit def forkRunnerExceptionWrites[T <: Throwable](implicit wrts:Writes[T]): Writes[ForkRunnerException[T]] = new Writes[ForkRunnerException[T]] {
    def writes(in:ForkRunnerException[T]):JsValue = in match {
      case x:PlayExceptionNoSource[T] => Json.obj("type" -> "no-source", "value" -> playExceptionNoSourceWrites[T](wrts).writes(x))
      case x:PlayExceptionWithSource[T] => Json.obj("type" -> "with-source", "value" -> playExceptionWithSourceWrites[T](wrts).writes(x))
    }
  }
  implicit def forkRunnerExceptionFormat[T <: Throwable](implicit reads:Reads[T], writes:Writes[T]): Format[ForkRunnerException[T]] = Format[ForkRunnerException[T]](forkRunnerExceptionReads, forkRunnerExceptionWrites)

  val throwableDeserializers = ThrowableDeserializers.empty
    .add[TemplateCompilationException]
    .add[RoutesCompilationException]
    .add[AssetCompilationException]
    .add[CompileFailedException]
    .add[CompilationException]
    .add[UnexpectedException]
    .add[ForkRunnerException[PlayException]]
    .add[ForkRunnerException[Throwable]]

  val formats: Seq[LocalRegisteredFormat] = List(LocalRegisteredFormat.fromFormat(playForkSupportResultFormat),
    LocalRegisteredFormat.fromFormat(compilationExceptionFormat),
    LocalRegisteredFormat.fromFormat(templateCompilationExceptionFormat),
    LocalRegisteredFormat.fromFormat(routesCompilationExceptionFormat),
    LocalRegisteredFormat.fromFormat(assetCompilationExceptionFormat),
    LocalRegisteredFormat.fromFormat(unexpectedExceptionFormat),
    LocalRegisteredFormat.fromFormat(forkRunnerExceptionFormat[PlayException]),
    LocalRegisteredFormat.fromFormat(forkRunnerExceptionFormat[Throwable]))
}

