package center.scala.sbk

import scala.sys.process.ProcessLogger

/**
  * Command Line Interpreter: allows to execute bash commands
  */
object Cli {
  def exe(path: String, cmds: Seq[String], args: Seq[String]): Unit = {
    val out = new StringBuilder
    val err = new StringBuilder

    val logger = ProcessLogger(
      (o: String) => out.append(o + "\n"),
      (e: String) => err.append(e + "\n")
    )

    sys.process.Process(cmds ++ args, new java.io.File(path)) ! logger

    // Prints status
    println(out)

    // Throws Exception is an error occurred
    if (err.nonEmpty) {
      err.toString().split("\n").foreach(Prompt.error)
      throw new Exception(s"Command-line process failed (see error messages above)")
    }
  }

  //def exeGit(path: String, cmds: Seq[String]): Boolean = {
  //  val p = sys.process.Process(cmds, new java.io.File(path)).run()
  //  p.exitValue() == 0
  //}
}