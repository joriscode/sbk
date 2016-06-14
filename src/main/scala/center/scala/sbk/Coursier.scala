package center.scala.sbk


import coursier.Fetch.Metadata
import rapture.fs._
import rapture.uri._
import coursier._

import scalaz.\/
import scalaz.concurrent.Task

object Coursier {
  private val repositories = Seq(
    Cache.ivy2Local,
    //Cache.ivy2Cache, // leads to errors when a dependency is in IvyCache but not its dependencies because it prevents to look in the cache of Coursier
    MavenRepository("https://repo1.maven.org/maven2")
  )

  private val local = Seq(
    Cache.ivy2Cache
  )

  private val fetch = Fetch.from(repositories, Cache.fetch())
  private val fetchLocal = Fetch.from(local, Cache.fetch())

  private def resolve(metadata: Metadata[Task], deps: List[center.scala.sbk.Dependency]): Resolution = {
    val preStart = Resolution(deps.map { d: center.scala.sbk.Dependency =>
      coursier.Dependency(Module(d.groupId, d.artifactId), d.version)
    }.toSet)

    preStart.process.run(fetch).run
  }

  private def run(deps: List[center.scala.sbk.Dependency], scalaVersion: String = scala.tools.nsc.Properties.versionString): Seq[FileError \/ java.io.File]  = {
    val resolution = resolve(fetch, deps)

    resolution.errors.foreach { e =>
      val dependency = e._1
      Prompt.error(s"Could not install ${dependency.module}:${dependency.version}")
    }

    Task.gatherUnordered(
      resolution.artifacts.map(Cache.file(_).run)
    ).run
  }

  // TODO can remove scalaVersion since Dependency should define it
  def installDependencies(scalaVersion: Option[String], libraries: List[Dependency]): Unit = scalaVersion match {
    case None => run(libraries)
    case Some(sv) => run(libraries, sv)
  }

  def cp(deps: List[center.scala.sbk.Dependency]): String = {
    val localArtifacts = run(deps)
    localArtifacts.filter(_.isRight).map(_.getOrElse(())).mkString(":")
  }

  private val coursierDir = File.homeDir / ".coursier" / "cache" / "v1" / "https"
  private val maven = coursierDir / "repo1.maven.org" / "maven2"
  private val sona = coursierDir / "oss.sonatype.org" / "content" / "repositories" / "releases"

  // TODO add sonatype
  def list(): List[Dependency] = {
    maven.descendants.filter(_.extension == Some("jar")).map(_.parent).map { x =>
      val y = x.elements.drop(maven.elements.size)
      val version = y.last
      val artifactId = y.dropRight(1).last
      val rest = y.dropRight(2).mkString(".")
      center.scala.sbk.Dependency(rest, artifactId, version)

    }.toList
  }

  /**
    * Exists locally
    */
  def exists(dependency: Dependency): Boolean = {
    resolve(fetchLocal, List(dependency)).errors.isEmpty
  }
}
