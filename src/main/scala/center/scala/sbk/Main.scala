package center.scala.sbk

import rapture.fs._
import rapture.uri._
import rapture.io._
import rapture.codec._
import encodings.`UTF-8`._

// TODO create API
// TODO handle scalaVersion for search
// TODO make uniform usage particularly the libraries fields
// find example, does publish is like curl?
// TODO benchmark

object Main extends App {
  try {
    Sbk.checkAccessToFiles()
    Scallop.whichCommand(args).exec()
    //Project.createCmdsFiles("lihaoyi", "Ammonite")
    //Project.createCmdsFiles("padurean", "scalatic")
    //sys.exit()

  } catch {
    case e: Throwable =>
      val message = e.getMessage
      val cause = e.getCause

      if (message != null) {
        Prompt.error(message)
      }

      if (cause != null) {
        Prompt.error(cause.toString)
      }

      Prompt.error(s"See ${Sbk.sbkDir}/last.log for full stacktrace")
      val log = Sbk.sbkDir / "last.log"
      val str = e.toString + "\n" + e.getStackTrace.mkString("\n") + "\n" + e.getCause
      str.copyTo(log)

      //sys.exit(1)
  }
}
