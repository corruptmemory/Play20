package play.runsupport

object Serializers {
  import play.api.libs.json._
  import play.api.libs.functional.syntax._
  import play.api.data.validation.ValidationError
  import play.runsupport.protocol._
  import PlayExceptions._
  import sbt.GenericSerializers._
  import sbt.protocol._

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

  val throwableDeserializers = ThrowableDeserializers.empty
                                  .add[CompileFailedException]
                                  .add[UnexpectedException]
                                  .add[TemplateCompilationException]
                                  .add[RoutesCompilationException]
                                  .add[AssetCompilationException]
                                  .add[CompilationException]

  val formats:Seq[LocalRegisteredFormat] = List(LocalRegisteredFormat.fromFormat(playForkSupportResultFormat),
                                              LocalRegisteredFormat.fromFormat(unexpectedExceptionFormat),
                                              LocalRegisteredFormat.fromFormat(compilationExceptionFormat),
                                              LocalRegisteredFormat.fromFormat(templateCompilationExceptionFormat),
                                              LocalRegisteredFormat.fromFormat(routesCompilationExceptionFormat),
                                              LocalRegisteredFormat.fromFormat(assetCompilationExceptionFormat))
}

