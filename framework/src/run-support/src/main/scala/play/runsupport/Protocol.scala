package play.runsupport

object protocol {
  import sbt.{ protocol => rcprotcol }
  final case class PlayForkSupportResult(analysis: rcprotcol.Analysis,
                                         dependencyClasspath: Seq[java.io.File],
                                         reloaderClasspath: Seq[java.io.File],
                                         allAssets: Seq[(String,java.io.File)],
                                         monitoredFiles: Seq[String],
                                         devSettings:Seq[(String, String)],
                                         docsClasspath: Seq[java.io.File])
}

