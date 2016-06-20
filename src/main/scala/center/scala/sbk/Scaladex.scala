package center.scala.sbk

import rapture.uri._
import rapture.net._
import rapture.io._
import rapture.json._
import jsonBackends.jawn._

object Scaladex {
  case class Request(group: Option[String], artifact: String) {
    case class FindResult(_1: Pagination, _2: List[Project])

    /**
      * Queries all projects on Scaldex corresponding to the Request
      *
      * @param limit NOT IMPLEMENTED, watchdog on the maximum of projects to collect
      * @return the corresponding projects
      */
    private def find(limit: Option[Int]): List[Project] = {
      /*
        * Queries Scaladex with the find API method
        *
        * @param pageIndex the index of the page [0, total - 1],
        *                  where total is the total indicated in the result of the query
        * @return returns the content of a page
        */
      def getPage(pageIndex: Int = 0): FindResult = {
        val scaladexUri: HttpQuery = uri"https://index.scala-lang.org"
        val url: String = "api/find?query=" + artifact + "&start=" + pageIndex
        val uri = scaladexUri.httpUrl / url

        val response = uri.httpGet()
        val str = response.slurp[Char]
        val json = Json.parse(str)
        json.as[FindResult]
      }

      /*
        * Recursively collects all the pages
        *
        * @param curPageIndex the current page index
        * @param acc the projects collected on previous pages
        * @return the projects queried on Scaladex
        */
      def helper(curPageIndex: PageIndex, acc: List[Project]): List[Project] = {
        val result = getPage(curPageIndex)

        if (curPageIndex < result._1.total - 1) {
          helper(curPageIndex + 1, acc ::: result._2)
        } else {
          acc ::: result._2
        }
      }

      helper(0, Nil)
    }


    /**
      * Returns the latest version of each matching libraries
      *
      * @param limit NOT IMPLEMENTED
      * @return
      */
    def latests(limit: Option[Int] = None): List[Dependency] = find(limit).flatMap { project =>
      project.artifacts.flatMap { artifact =>
        artifact.releases.lastOption.map { last =>
          val m = last.maven
          Dependency(m.groupId, m.artifactId, m.version)
        }
      }
    }

    /**
      * Returns matching libraries also filtered by version (partial match)
      *
      * @param version filters on version if defined
      * @param limit NOT IMPLEMENTED
      * @return
      */
    def search(version: Option[String], limit: Option[Int] = None): List[Dependency] = {
      val libs = find(limit).flatMap{ project =>
        project.artifacts.flatMap{ artifact =>
          artifact.releases.map{ release =>
            val m = release.maven
            Dependency(m.groupId, m.artifactId, m.version)
          }
        }
      }

      val filtered = group match {
        case None => libs
        case Some(g) => libs.filter(_.groupId.contains(g))
      }

      version match {
        case Some(v) => filtered.filter(_.version.contains(v))
        case None => filtered
      }
    }

  }


  /*
   *
   * BELOW THE DEFINITIONS OF SCALADEX STRUCTURE (Project, Artifact, Release, ...)
   *
   */


  type PageIndex = Int

  /**
    * Information about a page
    * @param current the index of the page
    * @param total the total number of pages
    */
  case class Pagination(current: PageIndex, total: Int)

  case class Artifact(
    reference: Artifact.Reference,
    releases: List[Release],
    deprecation: Option[Deprecation] = None
  )

  object Artifact {
    case class Reference(organization: String, name: String)
  }

  // see deprecated tag
  case class Deprecation(
    since: Option[ISO_8601_Date] = None
    //useInstead: Set[Artifact.Reference] = Set()
  )

  // typelevel/cats-core (scalajs 0.6, scala 2.11) 0.6.0
  case class Release(
    // famous maven triple: org.typelevel - cats-core_sjs0.6_2.11 - 0.6.0
    maven: MavenReference,
    // similar to maven but with a clean artifact name
    reference: Release.Reference,
    // human readable name (ex: Apache Spark)
    name: Option[String],
    description: Option[String],
    // potentially various dates because bintray allows republishing
    releaseDates: List[ISO_8601_Date],
    // availability on the central repository
    mavenCentral: Boolean,
    licenses: Set[License]
    //dependencies: Set[Release.Reference] = Set(),
    //reverseDependencies: Set[Release.Reference] = Set()
  ) {
    /*
    def sbtInstall = {
      val scalaJs = reference.targets.scalaJsVersion.isDefined
      val crossFull = reference.targets.scalaVersion.patch.isDefined

      val (artifactOperator, crossSuffix) =
        if (scalaJs)       ("%%%",                        "")
        else if(crossFull) (  "%", "cross CrossVersion.full")
        else               ( "%%",                        "")

      maven.groupId + artifactOperator + reference.artifact + "%" + reference.version + crossSuffix
    }
    def scalaDocURI: Option[String] = {
      if(mavenCentral) {
        import maven._
        // no frame
        // hosted on s3 at:
        // https://static.javadoc.io/$groupId/$artifactId/$version/index.html#package
        // HEAD to check 403 vs 200

        Some(s"https://www.javadoc.io/doc/$groupId/$artifactId/$version")
      } else None
    }
    */
  }
  object Release{
    case class Reference(
      organization: String,     // typelevel               | akka
      artifact: String         // cats-core               | akka-http-experimental
      //version: SemanticVersion // 0.6.0                   | 2.4.6
      //targets: ScalaTargets     // scalajs 0.6, scala 2.11 | scala 2.11
    )
  }

  // com.typesafe.akka - akka-http-experimental_2.11 - 2.4.6 | org.typelevel - cats-core_sjs0.6_2.11 - 0.6.0
  case class MavenReference(
    groupId: String,      // org.typelevel         | com.typesafe.akka
    artifactId: String,   // cats-core_sjs0.6_2.11 | akka-http-experimental_2.11
    version: String       // 0.6.0                 | 2.4.6
  )

  case class ScalaTargets(scalaVersion: SemanticVersion, scalaJsVersion: Option[SemanticVersion] = None)

  case class ISO_8601_Date(value: String) // 2016-05-20T12:48:52.533-04:00

  case class License(name: String, shortName: String, url: Option[String])

  object License {
    def spdx(id: String, name: String) =
      License(name, id, Some(s"https://spdx.org/licenses/$id.html"))

    // inspired by: https://github.com/NixOS/nixpkgs/blob/master/lib/licenses.nix#L1
    val Academic = spdx("AFL-3.0", "Academic Free License")
    val Affero = spdx("AGPL-3.0", "GNU Affero General Public License v3.0")
    val Apache2 = spdx("Apache-2.0", "Apache License 2.0")
    val Apple2_0 = spdx("APSL-2.0", "Apple Public Source License 2.0")
    val Beerware = spdx("Beerware", "Beerware License")
    val Bsd2Clause = spdx("BSD-2-Clause", """BSD 2-clause "Simplified" License""")
    val Bsd3Clause = spdx("BSD-3-Clause", """BSD 3-clause "New" or "Revised" License""")
    val BsdOriginal = spdx("BSD-4-Clause", """BSD 4-clause "Original" or "Old" License""")
    val CreativeCommonsZeroUniversal = spdx("CC0-1.0", "Creative Commons Zero v1.0 Universal")
    val CreativeCommonsAttributionNonCommercialShareAlike_2_0 = spdx("CC-BY-NC-SA-2.0", "Creative Commons Attribution Non Commercial Share Alike 2.0")
    val CreativeCommonsAttributionNonCommercialShareAlike_2_5 = spdx("CC-BY-NC-SA-2.5", "Creative Commons Attribution Non Commercial Share Alike 2.5")
    val CreativeCommonsAttributionNonCommercialShareAlike_3_0 = spdx("CC-BY-NC-SA-3.0", "Creative Commons Attribution Non Commercial Share Alike 3.0")
    val CreativeCommonsAttributionNonCommercialShareAlike_4_0 = spdx("CC-BY-NC-SA-4.0", "Creative Commons Attribution Non Commercial Share Alike 4.0")
    val CreativeCommonsAttributionShareAlike_2_5 = spdx("CC-BY-SA-2.5", "Creative Commons Attribution Share Alike 2.5")
    val CreativeCommonsAttribution_3_0 = spdx("CC-BY-3.0", "Creative Commons Attribution 3.0")
    val CreativeCommonsAttributionShareAlike_3_0 = spdx("CC-BY-SA-3.0", "Creative Commons Attribution Share Alike 3.0")
    val CreativeCommonsAttribution_4_0 = spdx("CC-BY-4.0", "Creative Commons Attribution 4.0")
    val CreativeCommonsAttributionShareAlike_4_0 = spdx("CC-BY-SA-4.0", "Creative Commons Attribution Share Alike 4.0")
    val Eclipse = spdx("EPL-1.0", "Eclipse Public License 1.0")
    val GPL1 = spdx("GPL-1.0", "GNU General Public License v1.0 only")
    val GPL1_Plus = spdx("GPL-1.0+", "GNU General Public License v1.0 or later")
    val GPL2 = spdx("GPL-2.0", "GNU General Public License v2.0 only")
    val GPL2Plus = spdx("GPL-2.0+", "GNU General Public License v2.0 or later")
    val GPl3 = spdx("GPL-3.0", "GNU General Public License v3.0 only")
    val GPL3Plus = spdx("GPL-3.0+", "GNU General Public License v3.0 or later")
    val ISC = spdx("ISC", "ISC License")
    val LGPL2 = spdx("LGPL-2.0", "GNU Library General Public License v2 only")
    // @deprecated("-", "-")
    val LGPL2_Plus = spdx("LGPL-2.0+", "GNU Library General Public License v2 or later")
    val LGPL2_1 = spdx("LGPL-2.1", "GNU Library General Public License v2.1 only")
    // @deprecated("-", "-")
    val LGPL2_1_Plus = spdx("LGPL-2.1+", "GNU Library General Public License v2.1 or later")
    val LGPL3 = spdx("LGPL-3.0", "GNU Lesser General Public License v3.0 only")
    // @deprecated("use LGPL3", "2.0rc2")
    val LGPL3_Plus = spdx("LGPL-3.0+", "GNU Lesser General Public License v3.0 or later")
    // Spdx.org does not (yet) differentiate between the X11 and Expat versions
    // for details see http://en.wikipedia.org/wiki/MIT_License#Various_versions
    val MIT = spdx("MIT", "MIT License")
    val MPL_1_0 = spdx("MPL-1.0", "Mozilla Public License 1.0")
    val MPL_1_1 = spdx("MPL-1.1", "Mozilla Public License 1.1")
    val MPL2 = spdx("MPL-2.0", "Mozilla Public License 2.0")
    val PublicDomain = License("Public Domain", "Public Domain", None)
    val Scala = License ("Scala License", "Scala License", Some("http://www.scala-lang.org/license.html"))
    val TypesafeSubscriptionAgreement = License("Typesafe Subscription Agreement", "Typesafe Subscription Agreement", Some("http://downloads.typesafe.com/website/legal/TypesafeSubscriptionAgreement.pdf"))
    val Unlicense = spdx("Unlicense", "The Unlicense")
    val W3C = spdx("W3C", "W3C Software Notice and License")
    val WTFPL = spdx("WTFPL", "Do What The F*ck You Want To Public License")

    def all = List(
      Academic,
      Affero,
      Apache2,
      Apple2_0,
      Beerware,
      Bsd2Clause,
      Bsd3Clause,
      BsdOriginal,
      CreativeCommonsZeroUniversal,
      CreativeCommonsAttributionNonCommercialShareAlike_2_0,
      CreativeCommonsAttributionNonCommercialShareAlike_2_5,
      CreativeCommonsAttributionNonCommercialShareAlike_3_0,
      CreativeCommonsAttributionNonCommercialShareAlike_4_0,
      CreativeCommonsAttributionShareAlike_2_5,
      CreativeCommonsAttribution_3_0,
      CreativeCommonsAttributionShareAlike_3_0,
      CreativeCommonsAttribution_4_0,
      CreativeCommonsAttributionShareAlike_4_0,
      Eclipse,
      GPL1,
      GPL1_Plus,
      GPL2,
      GPL2Plus,
      GPl3,
      GPL3Plus,
      ISC,
      LGPL2,
      LGPL2_Plus,
      LGPL2_1,
      LGPL2_1_Plus,
      LGPL3,
      LGPL3_Plus,
      MIT,
      MPL_1_0,
      MPL_1_1,
      MPL2,
      PublicDomain,
      Scala,
      TypesafeSubscriptionAgreement,
      Unlicense,
      W3C,
      WTFPL
    )
  }

  // typelevel/cats
  case class Project(
    reference: Project.Reference,
    artifacts: List[Artifact]
    // predefined keywords (ex: database)
    /*
    //keywords: List[String] = Nil,
    // http://stackoverflow.com/tags
    stackOverflowTags: List[String] = Nil,
    // @ handle (ex: @akkateam, @typelevel)
    twitter: Option[String] = None,
    // github repo associated with a gitter.im chat
    gitterIm: Boolean = false,
    // agglomerate of github organizations: lightbend(akka, play, ...), verizon(verizon, oncue), etc
    parentOrganization: Option[String] = None,

    // absolute url to a logo (ex: http://typelevel.org/img/logo.svg)
    logoImageUrl: Option[Url] = None
    */
  ) {
    def github = GithubRepo(reference.organization, reference.repository)
  }

  object Project{
    case class Reference(
      // github organization. ex: typelevel, akka, etc
      organization: String,

      // github repository. ex: cats, akka, etc
      repository: String
    )
  }

  case class Url(target: String)

  case class GithubRepo(organization: String, repo: String) {
    override def toString = s"$organization/$repo"
  }


  sealed trait PreRelease
  case class Milestone(value: Long) extends PreRelease
  case class ReleaseCandidate(value: Long) extends PreRelease
  case class OtherPreRelease(value: String) extends PreRelease

  case class SemanticVersion(
    major: Long, minor: Long = 0, patch: Option[Long] = None,
    preRelease: Option[PreRelease] = None,
    metadata: Option[String] = None
  ) {
    override def toString = {
      val patchPart = patch.map("." + _).getOrElse("")

      val preReleasePart = preRelease.map{
        case Milestone(d) => "M" + d.toString
        case ReleaseCandidate(d) => "RC" + d.toString
        case OtherPreRelease(v) => v.toString
      }.map("-" + _).getOrElse("")

      val metadataPart = metadata.map("+" + _).getOrElse("")

      major + "." + minor + patchPart + preReleasePart + metadataPart
    }
  }

  object SemanticVersion {
    implicit def ordering = new Ordering[SemanticVersion] {
      val LT = -1
      val GT =  1
      val EQ =  0

      val lcmp = implicitly[Ordering[Long]]
      val scmp = implicitly[Ordering[String]]
      val cmp = implicitly[Ordering[(Long, Long, Option[Long])]]

      def compare(v1: SemanticVersion, v2: SemanticVersion): Int = {
        def tupled(v: SemanticVersion) = {
          import v._
          (major, minor, patch)
        }

        val tv1 = tupled(v1)
        val tv2 = tupled(v2)

        def preCmp(pr1: Option[PreRelease], pr2: Option[PreRelease]): Int = {
          (pr1, pr2) match {
            case (None, None)                                               => EQ
            case (None, Some(_))                                            => GT
            case (Some(_), None)                                            => LT
            case (Some(ReleaseCandidate(rc1)), Some(ReleaseCandidate(rc2))) => lcmp.compare(rc1, rc2)
            case (Some(ReleaseCandidate(_))  , Some(Milestone(_)))          => GT
            case (Some(Milestone(_))         , Some(ReleaseCandidate(_)))   => LT
            case (Some(Milestone(m1))        , Some(Milestone(m2)))         => lcmp.compare(m1, m2)
            case (Some(OtherPreRelease(pr1)) , Some(OtherPreRelease(pr2)))  => scmp.compare(pr1, pr2)
            case (Some(OtherPreRelease(_))   , Some(Milestone(_)))          => LT
            case (Some(OtherPreRelease(_))   , Some(ReleaseCandidate(_)))   => LT
            case (Some(_)                    , Some(OtherPreRelease(_)))    => GT
          }
        }

        // Milestone < Release Candidate < Released
        if(cmp.equiv(tv1, tv2)) preCmp(v1.preRelease, v2.preRelease)
        else cmp.compare(tv1, tv2)
      }
    }
  }
}