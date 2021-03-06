/* NSC -- new Scala compiler
 * Copyright 2006-2012 LAMP/EPFL
 * @author  Martin Odersky
 */

package dotty.tools
package io

import java.net.URL
import scala.collection.{ mutable, immutable }
import dotc.core.Decorators.StringDecorator
import File.pathSeparator
import java.net.MalformedURLException
import Jar.isJarOrZip
import ClassPath._
import scala.Option.option2Iterable
import scala.reflect.io.Path.string2path
import language.postfixOps

/** <p>
 *    This module provides star expansion of '-classpath' option arguments, behaves the same as
 *    java, see [http://java.sun.com/javase/6/docs/technotes/tools/windows/classpath.html]
 *  </p>
 *
 *  @author Stepan Koltsov
 */
object ClassPath {

  /** Expand single path entry */
  private def expandS(pattern: String): List[String] = {
    val wildSuffix = File.separator + "*"

    /** Get all subdirectories, jars, zips out of a directory. */
    def lsDir(dir: Directory, filt: String => Boolean = _ => true) = {
      val files = synchronized(dir.list)
      files filter (x => filt(x.name) && (x.isDirectory || isJarOrZip(x))) map (_.path) toList
    }

    def basedir(s: String) =
      if (s contains File.separator) s.substring(0, s.lastIndexOf(File.separator))
      else "."

    if (pattern == "*") lsDir(Directory("."))
    else if (pattern endsWith wildSuffix) lsDir(Directory(pattern dropRight 2))
    else if (pattern contains '*') {
      val regexp = ("^%s$" format pattern.replaceAll("""\*""", """.*""")).r
      lsDir(Directory(pattern).parent, regexp findFirstIn _ isDefined)
    }
    else List(pattern)
  }

  /** Split classpath using platform-dependent path separator */
  def split(path: String): List[String] = (path split pathSeparator).toList filterNot (_ == "") distinct

  /** Join classpath using platform-dependent path separator */
  def join(paths: String*): String  = paths filterNot (_ == "") mkString pathSeparator

  /** Split the classpath, apply a transformation function, and reassemble it. */
  def map(cp: String, f: String => String): String = join(split(cp) map f: _*)

  /** Split the classpath, filter according to predicate, and reassemble. */
  def filter(cp: String, p: String => Boolean): String = join(split(cp) filter p: _*)

  /** Split the classpath and map them into Paths */
  def toPaths(cp: String): List[Path] = split(cp) map (x => Path(x).toAbsolute)

  /** Make all classpath components absolute. */
  def makeAbsolute(cp: String): String = fromPaths(toPaths(cp): _*)

  /** Join the paths as a classpath */
  def fromPaths(paths: Path*): String = join(paths map (_.path): _*)
  def fromURLs(urls: URL*): String = fromPaths(urls map (x => Path(x.getPath)) : _*)

  /** Split the classpath and map them into URLs */
  def toURLs(cp: String): List[URL] = toPaths(cp) map (_.toURL)

  /** Expand path and possibly expanding stars */
  def expandPath(path: String, expandStar: Boolean = true): List[String] =
    if (expandStar) split(path) flatMap expandS
    else split(path)

  /** Expand dir out to contents, a la extdir */
  def expandDir(extdir: String): List[String] = {
    AbstractFile getDirectory extdir match {
      case null => Nil
      case dir  => dir filter (_.isClassContainer) map (x => new java.io.File(dir.file, x.name) getPath) toList
    }
  }
  /** Expand manifest jar classpath entries: these are either urls, or paths
   *  relative to the location of the jar.
   */
  def expandManifestPath(jarPath: String): List[URL] = {
    val file = File(jarPath)
    if (!file.isFile) return Nil

    val baseDir = file.parent
    new Jar(file).classPathElements map (elem =>
      specToURL(elem) getOrElse (baseDir / elem).toURL
    )
  }

  /** A useful name filter. */
  def isTraitImplementation(name: String) = name endsWith "$class.class"

  def specToURL(spec: String): Option[URL] =
    try Some(new URL(spec))
    catch { case _: MalformedURLException => None }

  /** A class modeling aspects of a ClassPath which should be
   *  propagated to any classpaths it creates.
   */
  abstract class ClassPathContext {
    /** A filter which can be used to exclude entities from the classpath
     *  based on their name.
     */
    def isValidName(name: String): Boolean = true

    /** From the representation to its identifier.
     */
    def toBinaryName(rep: AbstractFile): String

    /** Create a new classpath based on the abstract file.
     */
    def newClassPath(file: AbstractFile): ClassPath

    /** Creators for sub classpaths which preserve this context.
     */
    def sourcesInPath(path: String): List[ClassPath] =
      for (file <- expandPath(path, false) ; dir <- Option(AbstractFile getDirectory file)) yield
        new SourcePath(dir, this)

    def contentsOfDirsInPath(path: String): List[ClassPath] =
      for (dir <- expandPath(path, false) ; name <- expandDir(dir) ; entry <- Option(AbstractFile getDirectory name)) yield
        newClassPath(entry)

    def classesAtAllURLS(path: String): List[ClassPath] =
      (path split " ").toList flatMap classesAtURL

    def classesAtURL(spec: String) =
      for (url <- specToURL(spec).toList ; location <- Option(AbstractFile getURL url)) yield
        newClassPath(location)

    def classesInExpandedPath(path: String): IndexedSeq[ClassPath] =
      classesInPathImpl(path, true).toIndexedSeq

    def classesInPath(path: String) = classesInPathImpl(path, false)

    // Internal
    private def classesInPathImpl(path: String, expand: Boolean) =
      for (file <- expandPath(path, expand) ; dir <- Option(AbstractFile getDirectory file)) yield
        newClassPath(dir)
  }

  class JavaContext extends ClassPathContext {
    def toBinaryName(rep: AbstractFile) = {
      val name = rep.name
      assert(endsClass(name), name)
      name.substring(0, name.length - 6)
    }
    def newClassPath(dir: AbstractFile) = new DirectoryClassPath(dir, this)
  }

  object DefaultJavaContext extends JavaContext {
    override def isValidName(name: String) = !isTraitImplementation(name)
  }

  private def endsClass(s: String) = s.length > 6 && s.substring(s.length - 6) == ".class"
  private def endsScala(s: String) = s.length > 6 && s.substring(s.length - 6) == ".scala"
  private def endsJava(s: String)  = s.length > 5 && s.substring(s.length - 5) == ".java"

  /** From the source file to its identifier.
   */
  def toSourceName(f: AbstractFile): String = {
    val name = f.name

    if (endsScala(name)) name.substring(0, name.length - 6)
    else if (endsJava(name)) name.substring(0, name.length - 5)
    else throw new FatalError("Unexpected source file ending: " + name)
  }
}

/**
 * Represents a package which contains classes and other packages
 */
abstract class ClassPath {
  type AnyClassRep = ClassPath#ClassRep

  /**
   * The short name of the package (without prefix)
   */
  def name: String

  /**
   * A String representing the origin of this classpath element, if known.
   * For example, the path of the directory or jar.
   */
  def origin: Option[String] = None

  /** A list of URLs representing this classpath.
   */
  def asURLs: List[URL]

  /** The whole classpath in the form of one String.
   */
  def asClasspathString: String

  /** Info which should be propagated to any sub-classpaths.
   */
  def context: ClassPathContext

  /** Lists of entities.
   */
  def classes: IndexedSeq[AnyClassRep]
  def packages: IndexedSeq[ClassPath]
  def sourcepaths: IndexedSeq[AbstractFile]

  /**
   * Represents classes which can be loaded with a ClassfileLoader
   * and / or a SourcefileLoader.
   */
  case class ClassRep(binary: Option[AbstractFile], source: Option[AbstractFile]) {
    def name: String = binary match {
      case Some(x)  => context.toBinaryName(x)
      case _        =>
        assert(source.isDefined)
        toSourceName(source.get)
    }
  }

  /** Filters for assessing validity of various entities.
   */
  def validClassFile(name: String)  = endsClass(name) && context.isValidName(name)
  def validPackage(name: String)    = (name != "META-INF") && (name != "") && (name.charAt(0) != '.')
  def validSourceFile(name: String) = endsScala(name) || endsJava(name)

  /**
   * Find a ClassRep given a class name of the form "package.subpackage.ClassName".
   * Does not support nested classes on .NET
   */
  def findClass(name: String): Option[AnyClassRep] =
    name.splitWhere(_ == '.', doDropIndex = true) match {
      case Some((pkg, rest)) =>
        packages find (_.name == pkg) flatMap (_ findClass rest)
      case _ =>
        classes find (_.name == name)
    }

  def findBinaryFile(name: String): Option[AbstractFile] =
    findClass(name).flatMap(_.binary)

  def sortString = join(split(asClasspathString).sorted: _*)

  override def equals(that: Any) = that match {
    case x: ClassPath => this.sortString == x.sortString
    case _            => false
  }
  override def hashCode = sortString.hashCode()
}

/**
 * A Classpath containing source files
 */
class SourcePath(dir: AbstractFile, val context: ClassPathContext) extends ClassPath {
  def name = dir.name
  override def origin = dir.underlyingSource map (_.path)
  def asURLs = if (dir.file == null) Nil else List(dir.toURL)
  def asClasspathString = dir.path
  val sourcepaths: IndexedSeq[AbstractFile] = IndexedSeq(dir)

  private def traverse() = {
    val classBuf   = immutable.Vector.newBuilder[ClassRep]
    val packageBuf = immutable.Vector.newBuilder[SourcePath]
    dir foreach { f =>
      if (!f.isDirectory && validSourceFile(f.name))
        classBuf += ClassRep(None, Some(f))
      else if (f.isDirectory && validPackage(f.name))
        packageBuf += new SourcePath(f, context)
    }
    (packageBuf.result, classBuf.result)
  }

  lazy val (packages, classes) = traverse()
  override def toString() = "sourcepath: " + dir.toString()
}

/**
 * A directory (or a .jar file) containing classfiles and packages
 */
class DirectoryClassPath(val dir: AbstractFile, val context: ClassPathContext) extends ClassPath {
  def name = dir.name
  override def origin = dir.underlyingSource map (_.path)
  def asURLs = if (dir.file == null) Nil else List(dir.toURL)
  def asClasspathString = dir.path
  val sourcepaths: IndexedSeq[AbstractFile] = IndexedSeq()

  // calculates (packages, classes) in one traversal.
  private def traverse() = {
    val classBuf   = immutable.Vector.newBuilder[ClassRep]
    val packageBuf = immutable.Vector.newBuilder[DirectoryClassPath]
    dir foreach { f =>
      if (!f.isDirectory && validClassFile(f.name))
        classBuf += ClassRep(Some(f), None)
      else if (f.isDirectory && validPackage(f.name))
        packageBuf += new DirectoryClassPath(f, context)
    }
    (packageBuf.result, classBuf.result)
  }

  lazy val (packages, classes) = traverse()
  override def toString() = "directory classpath: " + origin.getOrElse("?")
}

class DeltaClassPath(original: MergedClassPath, subst: Map[ClassPath, ClassPath])
extends MergedClassPath(original.entries map (e => subst getOrElse (e, e)), original.context) {
  // not sure we should require that here. Commented out for now.
  // require(subst.keySet subsetOf original.entries.toSet)
  // We might add specialized operations for computing classes packages here. Not sure it's worth it.
}

/**
 * A classpath unifying multiple class- and sourcepath entries.
 */
class MergedClassPath(
  val entries: IndexedSeq[ClassPath],
  val context: ClassPathContext)
extends ClassPath {
  def this(entries: TraversableOnce[ClassPath], context: ClassPathContext) =
    this(entries.toIndexedSeq, context)

  def name = entries.head.name
  def asURLs = (entries flatMap (_.asURLs)).toList
  lazy val sourcepaths: IndexedSeq[AbstractFile] = entries flatMap (_.sourcepaths)

  override def origin = Some(entries map (x => x.origin getOrElse x.name) mkString ("Merged(", ", ", ")"))
  override def asClasspathString: String = join(entries map (_.asClasspathString) : _*)

  lazy val classes: IndexedSeq[AnyClassRep] = {
    var count   = 0
    val indices = mutable.AnyRefMap[String, Int]()
    val cls     = new mutable.ArrayBuffer[AnyClassRep](1024)

    for (e <- entries; c <- e.classes) {
      val name = c.name
      if (indices contains name) {
        val idx      = indices(name)
        val existing = cls(idx)

        if (existing.binary.isEmpty && c.binary.isDefined)
          cls(idx) = existing.copy(binary = c.binary)
        if (existing.source.isEmpty && c.source.isDefined)
          cls(idx) = existing.copy(source = c.source)
      }
      else {
        indices(name) = count
        cls += c
        count += 1
      }
    }
    cls.toIndexedSeq
  }

  lazy val packages: IndexedSeq[ClassPath] = {
    var count   = 0
    val indices = mutable.AnyRefMap[String, Int]()
    val pkg     = new mutable.ArrayBuffer[ClassPath](256)

    for (e <- entries; p <- e.packages) {
      val name = p.name
      if (indices contains name) {
        val idx  = indices(name)
        pkg(idx) = addPackage(pkg(idx), p)
      }
      else {
        indices(name) = count
        pkg += p
        count += 1
      }
    }
    pkg.toIndexedSeq
  }

  private def addPackage(to: ClassPath, pkg: ClassPath) = {
    val newEntries: IndexedSeq[ClassPath] = to match {
      case cp: MergedClassPath => cp.entries :+ pkg
      case _                   => IndexedSeq(to, pkg)
    }
    new MergedClassPath(newEntries, context)
  }
  def show(): Unit = {
    println("ClassPath %s has %d entries and results in:\n".format(name, entries.size))
    asClasspathString split ':' foreach (x => println("  " + x))
  }
  override def toString() = "merged classpath " + entries.mkString("(", "\n", ")")
}

/**
 * The classpath when compiling with target:jvm. Binary files (classfiles) are represented
 * as AbstractFile. nsc.io.ZipArchive is used to view zip/jar archives as directories.
 */
class JavaClassPath(
  containers: IndexedSeq[ClassPath],
  context: JavaContext)
extends MergedClassPath(containers, context) { }

object JavaClassPath {
  def fromURLs(urls: Seq[URL], context: JavaContext): JavaClassPath = {
    val containers = {
      for (url <- urls ; f = AbstractFile getURL url ; if f != null) yield
        new DirectoryClassPath(f, context)
    }
    new JavaClassPath(containers.toIndexedSeq, context)
  }
  def fromURLs(urls: Seq[URL]): JavaClassPath =
    fromURLs(urls, ClassPath.DefaultJavaContext)
}
