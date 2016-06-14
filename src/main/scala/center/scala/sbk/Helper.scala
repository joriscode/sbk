package center.scala.sbk

import rapture.fs._

import scala.reflect.io.Path

object Helper {
  // TODO do a PR in Rapture
  def fsToPath(fs: FsUrl): String = {
    "/" + fs.elements.mkString("/")
  }
  def fsParent(fs: FsUrl): FsUrl = {
    FsUrl(fs.elements.dropRight(1))
  }
  def pathToFs(path: String): FsUrl = {
    if (path.startsWith("/")) {
      FsUrl(path.split("/").drop(1))

    } else {
      val file = path.split("/").toList match {
        case "." :: xs => getCurrentDirectory :: xs
        case ".." :: xs => fsParent(FsUrl(getCurrentDirectory.split("/"))).elements.toList ::: xs
        case "~" :: xs => File.homeDir.elements.toList ::: xs
        case f :: Nil => getCurrentDirectory :: List(f)
        case child :: xs => getCurrentDirectory :: child :: xs
        case Nil => throw new Exception("Empty string is not a valid path")
        // does not handle ~username
      }
      FsUrl(file)
    }
  }

  /**
    * It appeared the FsUrl.delete() did not correctly remove the directory.
    * This method is a workaround to FsUrl.delete()
    * Delete the directory pointed by fsUrl
    * @param fsUrl the directory to delete
    * @return
    */
  def deleteDir(fsUrl: FsUrl) = {
    val path: Path = Path (Helper.fsToPath(fsUrl))
    path.deleteRecursively()
  }

  def getCurrentDirectory = new java.io.File( "." ).getCanonicalPath.stripPrefix("/")
  def currentDir = Option(System.getProperty("user.dir")) // TODO use this one?
}
