package center.scala.sbk

import rapture.fs._
import rapture.uri._
import rapture.io._
import rapture.codec._
import encodings.`UTF-8`.implicitEncoding
import rapture.json._
import jsonBackends.jawn._

object Sbk {
  val sbkDir = File.homeDir / ".sbk"
  val toolName = "sbk"
  val toolVersion = "1.0.0"

  def checkAccessToFiles(): Unit = {
    if (!sbkDir.exists) sbkDir.mkdir()
    if (!sbkDir.exists) throw new Exception(s"$sbkDir could not be create")
    if (!sbkDir.readable) throw new Exception(s"$sbkDir is not readable")
    if (!sbkDir.writable) throw new Exception(s"$sbkDir is not writable")
  }

  // TODO Coursier does not need as many checks as Sbt => verify Coursier behavior when offline (use cache only)
  def existsInCache(dependency: Dependency): Boolean = Coursier.exists(dependency)

  // TODO What if .jar is deleted, so the map is erroneous => check when found it still exists + update map
  object Mapping {
    val mapFile = sbkDir / "libraries.json"

    if (! mapFile.exists) "{}".copyTo(mapFile)
    if (! mapFile.exists) throw new Exception(s"$mapFile cannot be created")

    populate()

    def populate(): Unit = {
      // TODO first update coursier-cache from ivy
      Coursier.list().foreach(store(_))
    }

    /** Locally searches for @lib.
      *
      * @return the list of dependency that @lib appears to partially match
      */
    def search(lib: InputLibrary): List[Dependency] = {
      val res = lib match {
        case InputLibrary(Some(group), "", _) => getAll.filter(_.groupId.contains(group))
        case InputLibrary(Some(group), n, _) => getByName(n).filter(_.groupId.contains(group))
        case InputLibrary(None, "", _) => getAll
        case InputLibrary(None, n, _) => getByName(n)
        //case InputLibrary(Some(group), None, _) => getAll.filter(_.groupId.contains(group))
        //case InputLibrary(None, None, _) => getAll
      }

      val set = lib.version match {
        case None => res
        case Some(v) => res.filter(_.version.contains(v))
      }
      set.toList.sortBy(_.version)(orderingVersion)
    }

    // TODO test ordering, do we need it?
    def orderingVersion: Ordering[String] = new Ordering[String] {
      private val ReleaseCandidate = """([0-9][0-9]*)[_\-][Rr][Cc]([0-9][0-9]*)""".r
      private val Milestone = """([0-9][0-9]*)[-_][Mm]([0-9][0-9]*)""".r
      private val Numbered = """([0-9][0-9]*)""".r

      private def difference(version1: String, version2: String): Int = {
        def cmp(l: Int, r: Int): Int = if (l == r) 0 else if (l > r) 1 else -1

        (version1, version2) match {
          case (Milestone(v1, m1), Milestone(v2, m2)) => if (v1 == v2) cmp(m1.toInt, m2.toInt) else cmp(v1.toInt, v2.toInt)
          case (ReleaseCandidate(v1, m1), ReleaseCandidate(v2, m2)) => if (v1 == v2) cmp(m1.toInt, m2.toInt) else cmp(v1.toInt, v2.toInt)
          case (Numbered(v1), Numbered(v2)) => cmp(v1.toInt, v2.toInt)
          case (ReleaseCandidate(_, _), Milestone(_, _)) => 1
          case (ReleaseCandidate(_, _), Numbered(_)) => -1
          case (Milestone(_, _), ReleaseCandidate(_, _)) => -1
          case (Milestone(_, _), Numbered(_)) => -1
          case (Numbered(v1), ReleaseCandidate(_, _)) => 1
          case (Numbered(v1), Milestone(_, _)) => 1
          case (v1, v2) => v1.compareTo(v2)
        }
      }
      def compare(v1: String, v2: String): Int = difference(v1, v2)
    }

    /** Returns from the map the libraries with an artifactId partially matching @name */
    private def getByName(name: String): Set[Dependency] = {
      val data = read()
      val artifactIds = data.keys.map{ artifactId =>
        (artifactId, artifactId.contains(name))
      }.filter(_._2).map(_._1).toList

      artifactIds.flatMap{ artifactId =>
        data.getOrElse(artifactId, throw new Exception("The key searched for should exist")).values.toSet.flatten
      }.toSet
    }

    /** Returns all the libraries from the map */
    private def getAll: Set[Dependency] = {
      val data = read()
      data.values.flatten.flatMap(_._2).toSet
    }

    /** Stores @map on mapFile */
    private def write(map: Map[String, Map[String, Set[Dependency]]]) = {
      Json.format(Json(map)).copyTo(mapFile)
    }

    /** Returns the map of libraries from mapFile */
    private def read(): Map[String, Map[String, Set[Dependency]]] = {
      Json.parse(mapFile.slurp[Char]).as[Map[String, Map[String, Set[Dependency]]]]
    }

    /** Stores @lib by projectName (for easier search by name) */
    def store(dep: Dependency): Unit = {
      val m = read()

      // TODO test
      val groups = m.get(dep.artifactId) match {
        case None =>
          Map(dep.groupId -> Set(dep))
        case Some(value) =>
          val libs = value.get(dep.groupId) match {
            case None => Set(dep)
            case Some(ls) => ls.+(dep)
          }
          Map(dep.groupId -> libs)
      }
      val updatedMap = m ++ Map(dep.artifactId -> groups)
      write(updatedMap)
    }
  }
}
