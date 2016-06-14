package center.scala.sbk

import center.scala.sbk.Scaladex.Request

/* latestScalaVersion <=> %% */
//case class MavenLibrary(groupId: String, artifactId: String, version: String, scope: Option[String], latestScalaVersion: Boolean) {
//  override def toString: String = "\"" + groupId + "\" % \"" + artifactId + "\" % \"" + version + "\""
//}


// TODO scope
// TODO %%
/** A (should be) correct key dependency.
  * ex: "com.typesafe.play" % "play-json_2.11" % "2.5.1"
  */
case class Dependency(groupId: String, artifactId: String, version: String) {
  override def toString: String = "\"" + groupId + "\" % \"" + artifactId + "\" % \"" + version + "\""

  /** Checks if this library exists locally and tries to fetch it from Maven if it does not.
    * Throws an exception if it the search on Maven fails to find one.
    * Prompts for choice if there are more than one possibility.
    */
  def existOrFetch(): Unit = {
    if (! Sbk.existsInCache(this)) {
      Prompt.info(s"$this is not installed, trying to fetch from Maven")
      val libs = Request(Some(groupId), artifactId).search(Some(version))
      val lib = Prompt.askForChoice(libs)
      lib.fetch()
    }
  }

  def fetch(): Unit = {
    if (Sbk.existsInCache(this)) {
      Prompt.info(s"Library $this is already installed")
      Sbk.Mapping.store(this)

    } else {
      Coursier.installDependencies(None, List(this))

      if (Sbk.existsInCache(this)) {
        Prompt.info(s"Library $this has been installed")
        Sbk.Mapping.store(this)

      } else {
        Error(s"Could not install the dependence $this").exec() // TODO Improve check of error (read sbt log...)
      }
    }
  }
}

// TODO should handle scalaVersion + scope
case class InputLibrary(group: Option[String], artifact: String, version: Option[String])  {
  override def toString: String = group.getOrElse("any_group") + " % " + artifact + " % " + version.getOrElse("any_version")

  /** Searches for this pseudo-library on Maven.
    * Queries only the latest versions from Maven if version is None.
    * Otherwise, filters on the version.
    *
    * @return the corresponding libraries order by version (scaladex definition)
    */
  def searchOnline(): List[Dependency] = {
    Prompt.info("search on Scaladex")

    // TODO query scaladex if accessible

    version match {
      case Some(v) =>
        Request(group, artifact).search(version)
      case None =>
        //Request(group, artifact).latest()
        Request(group, artifact).search(None)
    }
  }

  // TODO take care of the order
  /** Searches for this pseudo-library in local cache.
    *
    * @return the matching libraries
    */
  def searchLocally(): List[Dependency] = {
    Prompt.info("search in cache")
    Sbk.Mapping.search(this)
  }

  /** Returns the corresponding library.
    * Prompts a selection if there is many possibilities.
    * Queries Scaladex and searches locally if there is no response.
    *
    * @return the unique or selected matching library
    */
  def searchAndSelect(): Dependency = {
    val online = searchOnline()

    val res = if (online.isEmpty) {
      searchLocally()

    } else {
      online
    }

    if (res.isEmpty) {
      throw new Exception(s"Could not find a corresponding library. Refine your search.")
    } else {
      Prompt.askForChoice(res)
    }
  }
}

object InputLibrary {
  // TODO define format of args
  /** Defines a pseudo-library from @args
    *
    * @param args ...
    **/
  def fromString(args: List[String]): InputLibrary = args.mkString.split("%").toList.filter(_.nonEmpty) match {
    case libName :: Nil => InputLibrary(None, libName, None)
    case group :: libName :: version :: Nil => InputLibrary(Some(group), libName, Some(version))
    case group :: libName :: Nil => InputLibrary(Some(group), libName, None)
    case _ => throw new Exception(s"Could not parse the given arguments in a valid definition of a library")
    // use TempLib(Option(group).filter(_.trim.nonEmpty), Option(libName).filter(_.trim.nonEmpty), Option(version).filter(_.trim.nonEmpty))
  }
}

