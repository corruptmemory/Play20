/*
 * Copyright (C) 2009-2014 Typesafe Inc. <http://www.typesafe.com>
 */
package play.runsupport

import sbt._
import java.io.File
import Path._

/**
 * A ClassLoader for serving assets.
 *
 * Serves assets from the given directories, at the given prefix.
 *
 * @param assets A list of assets directories, paired with the prefix they should be served from.
 */
class AssetsClassLoader(parent: ClassLoader, assets: Seq[(String, File)]) extends ClassLoader(parent) {
  override def findResource(name: String) = {
    println(s"LOOKING FOR: $name")
    val r = assets.collectFirst {
      case (prefix, dir) if exists(name, prefix, dir) =>
        (dir / name.substring(prefix.length)).toURI.toURL
    }.orNull
    println(s"FOUND[$name]: $r")
    r
  }

  def exists(name: String, prefix: String, dir: File) = {
    println(s"CHECKING FOR: $name, $prefix, $dir")
    val r = name.startsWith(prefix) && (dir / name.substring(prefix.length)).isFile
    println(s"IS FILE[$name, $prefix, $dir]: $r")
    r
  }
}
