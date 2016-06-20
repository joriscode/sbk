package center.scala.sbk

import java.util.Base64

import rapture.core._
import rapture.net._
import rapture.fs._
import rapture.json._
import jsonBackends.jawn._
import rapture.codec._
import encodings.`UTF-8`.implicitEncoding
import rapture.data.Formatter
import rapture.uri._
import rapture.io._
import rapture.mime.MimeTypes

// implement flat storage (no org/repo/hierarchy/script.sh)
object Github {
  /**
    * Directory containing all the script downloaded from Github
    * */
  private val directory = Sbk.sbkDir / "github"

  if (! directory.exists) directory.mkdir()
  if (! directory.exists) throw new Exception(s"$directory cannot be created")

  /**
    * File containing the token allowing access to the user Github
    */
  private val tokenFile = Sbk.sbkDir / "oAuth2.user"
  val defaultUser = Json.format(Json(Pool("", "", Some(""))))
  if (! tokenFile.exists) defaultUser.copyTo(tokenFile)
  if (! tokenFile.exists) throw new Exception(s"$tokenFile cannot be created")

  /*
  /**
    * Convenient class for representing the personal information of the user.
    * @param pool the github repo containing the scripts of the user
    * @param token the personal API access token of Github
    */
  private case class UserPool(pool: Pool, token: String)
  */

  private val user: Pool = {
    if (tokenFile.readable) {
      val str = tokenFile.slurp[Char]
      val json = Json.parse(str)
      val user = json.as[Pool]

      if (user.org.nonEmpty && user.repo.nonEmpty && user.token.isDefined && user.token.get.nonEmpty) {
        user
      } else {
        throw new Exception(s"Define your private repo at $tokenFile")
      }
    } else {
      throw new Exception(s"$tokenFile is not readable")
    }
  }

  def getUserPool: Pool = user

  private val oAuth2 = Map("Authorization" -> ("token " + user.token.get.trim)) // TODO does not work Initialization error

  case class Pool(org: String, repo: String, token: Option[String]) {
    /**
      * The url to the Github repo
      */
    val url = uri"https://api.github.com/repos/$org/$repo"

    /**
      * Returns the path where to write the script.
      * Returns .sbk/github/org/repo/name
      *
      * @param name the name of the script with its extension.
      * @return the path
      */
    def destPath(name: String): FsUrl = {
      val directory = File.homeDir / ".sbk" / "github" // TODO why does not work with Github.directory?
      val dir = directory / org / repo
      dir.mkdir(true)
      if (! dir.exists) throw new Exception(s"Could not create the directory $dir")
      dir / name
    }

    /**
      * Information and content of a file on Github.
      */
    case class RefFile(
      `type`: String,
      name: String,
      path: String,
      sha: String,
      size: Int,
      url: String,
      download_url: String,
      encoding: String,
      content: String
    ) {

      /**
        * Checks if this RefFile is of type file
        *
        * @return true if this.type == "file"
        */
      def isFile: Boolean = this.`type` == "file"
    }

    /**
      * Returns the RefFile of @name.
      *
      * @return the path to the requested file
      */
    def get(name: String): RefFile = {
      val response = (url.httpUrl / ("contents" + "/" + name)).httpGet()

      import rapture.core.modes.returnTry._
      val refFile = for {
        str <- response.slurp[Char]
        json <- Json.parse(str)
        rf <- json.as[RefFile]
      } yield rf

      import scala.util.{Success, Failure}
      refFile match {
        case Success(file) => file
        case Failure(_) => throw new Exception(s"Could not get the file $name on pool $this")
      }
    }

    /**
      * Returns the Github sha1 of @name.
      *
      * @param name path to the file on this repo
      * @return Some(sha1) if is a file otherwise None
      */
    def getSha(name: String): Option[String] = {
      val rf = get(name)
      rf.isFile match {
        case false => None
        case true => Some(rf.sha)
      }
    }

    /**
      * Gets the content of the script @name.
      *
      * @param name path to the script of the script for this repo
      * @return Some(content) or None if @name is not a file
      */
    def download(name: String): Option[String] = {

      // could certain use decode64(rf.content)
      def downloadContent(urlString: String): String = {
        val response = urlString.as[HttpUrl].httpGet()
        response.slurp[Char]
      }

      val file = get(name)

      if (file.isFile) {
        Some(downloadContent(file.download_url))

      } else {
        None
      }
    }

    // implemented in version 2.0.0-M6 of rapture
    implicit def jsonPostType(implicit formatter: Formatter[JsonAst] { type Out = String }): PostType[Json] = new PostType[Json] {
      def contentType = Some(MimeTypes.`application/json`)
      def sender(content: Json): Input[Byte] =
        Json.format(content).input[Byte]
    }

    /**
      * Encodes in base64.
      *
      * @param str the string to be encoded
      * @return the encoded string
      */
    private def encoder64(str: String): String = Base64.getEncoder.encodeToString(str.getBytes("utf-8"))

    /**
      * Uploads a script.
      *
      * @param fs the path to the script to be uploaded.
      */
    def upload(fs: FsUrl): Unit = {
      /*
        * A convenient class for gathering and converting to Json the content to be pushed.
        */
      case class PushFile(message: String, content: String)

      if (! fs.exists) throw new Exception(s"The file $fs does not exist. Could not upload it.")
      val content = encoder64(fs.slurp[Char])
      val post = PushFile(s"Push new script ${fs.filename}", content)

      if (this.exists(fs.filename)) throw new Exception(s"Could not upload ${fs.filename} seems to already exists on $this")

      val httpUrl: HttpUrl = url.httpUrl / ("contents/" + fs.filename)
      val status: HttpResponse = httpUrl.httpPut(Json(post), oAuth2)

      if (status.status  == 409) Prompt.error("Github returns status 409 Conflict - This file might already exist")
      if (status.status != 201) throw new Exception(s"Could not upload $fs. HttpPut status ${status.status}")
    }

    /**
      * Updates an existing script.
      * Throws an exception if the status is not 200.
      *
      * @param fs the location of the file
      */
    def update(fs: FsUrl): Unit = {
      /*
        * A convenient class for gathering and converting to Json the content to be pushed.
        */
      case class UpdateFile(message: String, content: String, sha: String)

      if (! fs.exists) throw new Exception(s"The file $fs does not exist. Could not update it.")

      val (httpUrl, sha) = getUrl(fs.filename) match {
        case Some(x) => (x.url.as[HttpUrl], x.sha)
        case None => throw new Exception(s"Could not find $fs on $this")
      }
      val content = encoder64(fs.slurp[Char])
      val post = UpdateFile(s"Push update of script ${fs.filename}", content, sha)

      val status: HttpResponse = httpUrl.httpPut(Json(post), oAuth2)
      if (status.status != 200) throw new Exception(s"Could not update $fs. HttpPut status ${status.status}")
    }

    /**
      * Returns the url of @file in this Pool.
      *
      * @param file the path of the file in this Github repo
      * @return the corresponding InfoFile
      */
    private def getUrl(file: String): Option[InfoFile] = this.listFiles().find(_.name.contains(file))

    /**
      * Returns true if the file exists in the repo
      *
      * @param file path to the file on the repo
      * @return existance of the file
      */
    def exists(file: String): Boolean = getUrl(file).isDefined

    // could be private if listFiles does not return InfoFiles
    /**
      * Information about a file on Github.
      */
    case class InfoFile(
      `type`: String,
      name: String,
      path: String,
      sha: String,
      size: Int,
      url: String,
      download_url: Option[String]
    )

    /**
      * Lists the files in this Pool.
      *
      * @return the corresponding InfoFiles
      */
    def listFiles(): List[InfoFile] = {
      val u = url.httpUrl / "contents"
      val response = u.httpGet()
      val str = response.slurp[Char]
      val json = Json.parse(str)

      json.as[List[InfoFile]]
    }

    override def toString: String = org + "/" + repo + "/"

    /**
      * Another toString, not using slash to be compatible for file naming
      *
      * @return a string for file naming ending with a dash so it can be prepend to the filename
      */
    def toName = org + "-" + repo + "-"
  }

  object Listing {
    val listing = Sbk.sbkDir / "pools.json"
    if (! listing.exists) "{}".copyTo(listing)
    if (! listing.exists) throw new Exception(s"$listing cannot be created")

    def get(alias: String) = read().get(alias)
    def exists(alias: String): Boolean = read().get(alias).isDefined
    def add(alias: String, pool: Pool): Unit = write(read() + (alias -> pool))
    def remove(alias: String): Unit = write(read() - alias)

    private def read(): Map[String, Pool] = {
      Json.parse(listing.slurp[Char]).as[Map[String, Pool]]
    }

    private def write(list: Map[String, Pool]) = {
      Json.format(Json(list)).copyTo(listing)
    }

  }
}
