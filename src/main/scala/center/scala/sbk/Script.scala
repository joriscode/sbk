package center.scala.sbk

import center.scala.sbk.Github.Pool
import rapture.uri._
import rapture.fs._
import rapture.io._
import rapture.codec._
import encodings.`UTF-8`.implicitEncoding
import rapture.json._
import jsonBackends.jawn._

object Script {
  /**
    * Extension of script files
    */
  val extension = "sbk"

  object Listing {
    import com.roundeights.hasher.Implicits._
    import scala.language.postfixOps

    /**
      * Information about registered file
      *
      * @param path location of the script
      * @param sha1 hash of the script
      * @param source None if local else Some(url)
      * @param main main object to be called. Ex: Main.main(args)
      */
    case class RegisteredFile(path: FsUrl, sha1: String, source: Option[Pool], sourcePath: Option[String], main: String, classpath: String) {
      /**
        * Checks the sha1 sum of this file.
        *
        * @return true if the content did not change
        */
      def checkSha1(): Boolean = {
        path.slurp[Char].sha1 hash= sha1
      }
    }

    /**
      * File containing the alias and the information about registered scripts
      */
    val listing = Sbk.sbkDir / "aliases.json"
    if (! listing.exists) "{}".copyTo(listing)
    if (! listing.exists) throw new Exception(s"$listing cannot be created")

    /**
      * Directory storing the compiles classes of registered scripts
      */
    val classesDir = Sbk.sbkDir / "classes"
    if (! classesDir.exists) classesDir.mkdir()
    if (! classesDir.exists) throw new Exception(s"$classesDir cannot be created")

    def pathToClassesDir(alias: String): String = Helper.fsToPath(fsToClassesDir(alias))
    def fsToClassesDir(alias: String): FsUrl = classesDir / alias

    private def hash(path: FsUrl): String = path.slurp[Char].sha1.hex

    /**
      * Updates the hash, the classpath of the script
      * @param alias alias of the script
      */
    def updateMetadata(alias: String, classpath: String) = {
      val data = read()
      data.get(alias) match {
        case None =>
          throw new Exception(s"Could not update the metadata of the script $alias when recompiling")
        case Some(rf) =>
          val tmpData = data - alias
          val newData = data + (alias -> RegisteredFile(rf.path, hash(rf.path), rf.source, rf.sourcePath, rf.main, classpath))
          write(newData)
      }
    }

    /**
      * Compiles a script and stores the classes in .sbk/classes/alias.
      *
      * @param alias alias of the script
      * @param script path to the script
      * @return the classpath
      */
    def compile(alias: String, script: FsUrl): String = {
      val dir = fsToClassesDir(alias)
      if (! dir.exists) dir.mkdir()
      if (! dir.exists) throw new Exception(s"Cannot create $dir to store the compiled classes")

      val classesDest = pathToClassesDir(alias)
      val classPath = Coursier.cp(Script.extractDependencies(script))

      Cli.exe(Helper.fsToPath(dir), Seq("scalac", "-d", classesDest, "-cp", classPath, Helper.fsToPath(script)), Nil)
      classPath
    }

    // TODO Caution if it fails, we delete the working version
    def reCompile(alias: String, script: FsUrl) = {
      val classDir = fsToClassesDir(alias)
      Helper.deleteDir(classDir)
      val classpath = compile(alias, script)
      updateMetadata(alias, classpath)
    }

    def updateMain(alias: String, rf: RegisteredFile, main: String) = {
      val data = read()
      val rf = data.getOrElse(alias, throw new Exception(s"The script alias $alias does not exist"))
      val tempData = data - alias
      val newData = tempData ++ Map(alias -> RegisteredFile(rf.path, rf.sha1, rf.source, rf.sourcePath, main, rf.classpath))
      write(newData)
    }

    def update(alias: String, rf: RegisteredFile) = {
      rf.source match {
        case None => Prompt.error(s"Could not update $alias because it is not a file downloaded from Github")
        case Some(pool) =>
          val name = rf.path.filename
          val refFile = pool.get(name)
          if (refFile.sha == rf.sha1)
            Prompt.info(s"$alias is up-to-date")
          else {
            Script.Listing.remove(alias)
            pool.download(name)
          }
      }
    }

    def updateAll() = {
      Script.Listing.all().toList.filter(_._2.source.isDefined).foreach{ case (k, v) => update(k, v) }
    }

    /**
      * Add a script to the list of registered scripts.
      * Does a sha1 checksum of the script file.
      * Compile the script.
      *
      * @param alias the alias for the script
      * @param path location of the script
      * @param pool None if local, Some(pool) where pool is the Github source repo
      * @param srcPath path to the script on the repo
      * @param main main object
      * @return
      */
    def add(alias: String, path: FsUrl, pool: Option[Pool], srcPath: Option[String], main: String) = {
      if (exists(alias)) {
        Prompt.warn(s"Could not add $alias -> $path because this alias is already taken")

      } else {
        Prompt.info(s"Compile script $path")
        val classpath = compile(alias, path)
        val data = read()
        val value = RegisteredFile(path, hash(path), pool, srcPath, main, classpath)
        val updatedListing = data ++ Map(alias -> value)
        write(updatedListing)
      }
    }

    def remove(alias: String) = {
      val data = read()
      val updatedListing = data - alias
      write(updatedListing)
      val fs = fsToClassesDir(alias)
      fs.children.foreach(_.delete())
      fs.delete()
    }

    def changed(name: String): Boolean = get(name) match {
      case None => throw new Exception(s"the alias $name does not exist")
      case Some(rf) => rf.checkSha1()
    }

    def all(): Map[String, RegisteredFile] = read()

    def exists(alias: String): Boolean = getPath(alias).isDefined

    /**
      * Returns the location of the file
      *
      * @param alias the alias of the scrip
      * @return
      */
    def getPath(alias: String): Option[FsUrl] = get(alias) match {
      case None => None
      case Some(rf) => Some(rf.path)
    }

    def get(alias: String): Option[RegisteredFile] = read().get(alias)

    private def write(map: Map[String, RegisteredFile]) = {
      Json.format(Json(map)).copyTo(listing)
    }

    private def read(): Map[String, RegisteredFile] = {
      Json.parse(listing.slurp[Char]).as[Map[String, RegisteredFile]]
    }
  }

  private val openingMark = "/* sbk\n"
  private val closingMark = "*/"

  def fastParse(content: String): (String, List[Dependency], String) = {
    import fastparse.all._
    import fastparse.core.Parsed
    val anyString = P( CharIn(".", "_", "-", 'a' to 'z', 'A' to 'Z', '0' to '9').rep )
    val space = P(" ")

    val dotOrDigit = P( CharIn(".", '0' to '9'))
    val scalaVersionParser = P(space.rep ~ "scala version" ~ space.rep ~ dotOrDigit.rep.! ~ space.rep ~ "\n")

    val openingParser = P(space.rep ~ openingMark)
    val closingParser = P(space.rep ~ closingMark ~ space.rep)

    val dependency = P(space.rep ~ "library" ~ space.rep ~ "\"" ~ anyString.! ~ "\"" ~ space ~ "%" ~ "%".!.? ~ space ~ "\"" ~ anyString.! ~ "\"" ~ " % " ~ "\"" ~ anyString.! ~ "\"" ~ space.rep ~ "\n")

    /* Intermediate Representation of Dependency that is used to resolve artifact => artifactId
      * Define the scalaVersion if the dependency key doesn't (see %%)
      */
    case class IRDependency(groupId: String, artifact: String, version: String, scalaVersionFlag: Boolean)

    val sbkHeaderParser = P(openingParser ~ scalaVersionParser ~ dependency.map{ case (g, latestScalaVersion, a, v) =>
      IRDependency(g, a, v, latestScalaVersion.isDefined)
    }.rep ~ closingParser)

    val headerParser = P(AnyChar.rep) // TODO allow header
    val bodyParser = P(AnyChar.rep)

    val header = Some("")
    val scriptParser = P(sbkHeaderParser.? ~ bodyParser.!.?)
    val Parsed.Success(((sbkHeader: Option[(String, Seq[IRDependency])], body: Option[String])), successIndex) = scriptParser.parse(content)

    val deps = sbkHeader match {
      case None =>
        Prompt.warn("Not sbk header declared")
        Nil
      case Some((scalaVersion, irDeps)) =>
        val regex = """^(\d\.\d\d)\.?\d?$""".r
        val sV  = scalaVersion match {
          case regex(s) => s
          case _ => throw new Exception("Invalid format of scala version given in the sbk header of the script")
        }

        irDeps.map{ ir: IRDependency =>
          val suffix = if (ir.scalaVersionFlag) "_" + sV else ""
          Dependency(ir.groupId, ir.artifact + suffix, ir.version)
        }
    }
    (header.getOrElse(""), deps.toList, body.getOrElse(""))
  }

  private def parse(script: FsUrl): (String, List[Dependency], String) = {
    if (! script.readable) throw new Exception(s"$script is not readable")
    val content = script.slurp[Char]
    fastParse(content)
  }

  def extractDependencies(file: FsUrl): List[Dependency] = parse(file)._2

  def add(libs: List[Dependency], file: FsUrl): Unit = {
    val (header, deps, body) = parse(file)
    val updatedDeps = deps ++ libs
    (header + openingMark + updatedDeps.distinct.mkString("\n") + "\n" + closingMark + "\n" + body).copyTo(file)
  }

  def template(dest: FsUrl, libs: List[Dependency]): Unit = {
    val notInstalled = libs.filterNot(Sbk.existsInCache)
    notInstalled.foreach{ lib => Prompt.warn(s"Library is not yet installed $lib") }

    if (notInstalled.nonEmpty) {
      val install = Prompt.ask("Do you want to install the missing library?")

      if (install) {
        Coursier.installDependencies(None, notInstalled)
      }
    }

    val libraries = libs.map("library " + _)

    val header =
      s"""|/* sbk
          |scala version ${scala.tools.nsc.Properties.versionString}
          |${libraries.mkString("\n")}
          |*/
          |//Main.main(args) // Main class. Uncomment to run the script without compilation
          |
          |// scala code here""".stripMargin

    val dummyBody =
      s"""
         |object Main extends App {
         |  println("Hello, world!")
         |}
         |""".stripMargin

    (header + dummyBody).copyTo(dest)
    Prompt.info(s"Template created at $dest")
    Prompt.info("To declare a dependency, add in the header of the script: library <dependency key>")
  }
}
