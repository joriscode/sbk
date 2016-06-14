package center.scala.sbk

object Prompt {
  val packageManagerName = "sbk"
  // for interactive shell
  //final case class State(commands: List[Command], next: State, history: Any) {
  //  def prompt(): Unit = {
  //  }
  //}

  /*
  // TODO improve
  /** Displays the list of possibilities and prompts for the selection.
    * Fails if list is empty.
    */
  def askForChoice(deps: List[String]): String = {
    def promptLoop(max: Int): Int = {
      display(s"Select one dependency (number between 0 and $max}")
      val selection = scala.io.StdIn.readInt()

      if (selection >= 0 && selection <= max) {
        selection
      } else {
        promptLoop(max)
      }
    }

    deps.length match {
      case 0 =>
        error("No choice existing")
        throw new Exception("askForChoice but no choice")
      case 1 =>
        display(s"One choice: ${deps.head}")
        deps.head
      case _ =>
        deps.zipWithIndex.foreach { case (lib, nb) => display(s"$nb: $lib") }
        deps(promptLoop(deps.length - 1))
    }
  }
  */


  // TODO ? The list should be ordered
  /** Displays the list of possibilities and prompts for the selection.
    * Fails if list is empty.
    */
  def askForChoice(deps: List[Dependency]): Dependency = {
    def promptLoop(max: Int): Int = {
      display(s"Select one dependency (number between 0 and $max}")
      val selection = scala.io.StdIn.readInt()

      if (selection >= 0 && selection <= max) {
        selection
      } else {
        promptLoop(max)
      }
    }

    deps.length match {
      case 0 =>
        error("No choice existing")
        throw new Exception("askForChoice but no choice")
      case 1 =>
        display(s"One choice: ${deps.head}")
        deps.head
      case _ =>
        deps.zipWithIndex.foreach { case (lib, nb) => display(s"$nb: $lib") }
        deps(promptLoop(deps.length - 1))
    }
  }

  /** Prompts a message and waits for a boolean input
    *
    * @param msg the question
    **/
  def ask(msg: String): Boolean = {
    display(msg)
    scala.io.StdIn.readBoolean()
  }

  var options: Set[Char] = Set()

  def display(msg: String) = Console.println(msg)

  def newLine() = Console.println

  def info(msg: String) = display(s"[Info] $msg")

  def warn(msg: String) = if (options.contains('r')) {
    display(s"[Warn] $msg")
  } else {
    display("[" + Console.YELLOW + "Warn" + Console.RESET + "] " + msg)
  }

  def error(msg: String) = if (options.contains('r')) {
    display(s"[Error] $msg")
  } else {
    display ("[" + Console.RED + "Error" + Console.RESET + "] " + msg)
  }


  /** Displays the list of dependency. Take ordered list!
    *
    * @param res the list of dependency
    * @param last only prints the lastest version of pair (groupId, artifactId)
    * @param url prints the Scaldex url of the corresponding library
    */
  def displayResults(res: List[Dependency], last: Boolean, url: Boolean): Unit = {
    val raw = options.contains('r')

    var counter = 0
    val byGroup = res.groupBy(_.groupId)
    byGroup.keys.foreach{ group =>
      if (! raw) Prompt.display(s"* $group")
      val byProject = byGroup.get(group).get.groupBy(_.artifactId)

      byProject.keys.foreach{ project =>
        if (! raw) Prompt.display(s"** $project")

        // TODO is it kept?
        /* Should stay ordered since res must be ordered */
        val versions = byProject.get(project).get

        val sep = if (raw) {
          ""
        } else {
          "---- "
        }

        val link = if (url) {
          " --> http://..."
        } else {
          ""
        }

        if (last) {
          Prompt.display(sep + versions.head + link)
          counter+=1
        } else {
          versions.foreach{ x => Prompt.display(sep + x + link) }
          counter+=versions.length
        }

        /* Separate different artifactIds by one newline */
        if (! raw) Prompt.newLine
      }

      /* Separate different groupIds by two newline */
      if (! raw) Prompt.newLine
      if (! raw) Prompt.newLine
    }

    if (! raw) Prompt.info(s"Found ${counter} different releases")
  }

  object Parser {

    /*
    // TODO ???
    case class NamedFunction[T, V](f: T => V, name: String) extends (T => V) {
      def apply(t: T) = f(t)

      override def toString() = name
    }
    */

    /** Parse short options until a non-option word.
      * Ex: -to word return (List(word), Set('t', 'o'))
      * Caution: --hello beautiful world => Options are '-', 'h', 'e' ...
      */
    def parseOptions(args: List[String], acc: Set[Char]): (List[String], Set[Char]) = args match {
      case head :: xs if head.startsWith("-") => parseOptions(xs, acc ++ head.stripPrefix("-").toCharArray.toSet)
      case _ => (args, acc)
    }
  }
}
