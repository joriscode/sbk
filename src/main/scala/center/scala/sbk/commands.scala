package center.scala.sbk

import center.scala.sbk.Github.Pool
import rapture.fs._
import rapture.io._
import rapture.codec._
import encodings.`UTF-8`.implicitEncoding

sealed trait Command {
  def exec(): Unit
}

case class Search(lib: InputLibrary, cache: Boolean, last: Boolean, url: Boolean) extends Command {
  override def exec(): Unit = {
    Prompt.info(s"search for $lib")

    if (lib.group.isEmpty && lib.artifact.isEmpty && lib.version.isEmpty) {
      Prompt.error(s"$lib has no group, no project, no version defined for the search")

    } else {
      val res = if (cache) {
        lib.searchLocally()

      } else {
        lib.searchOnline()
      }

      if (res.isEmpty) {
        if (cache) {
          Prompt.warn(s"Did not find any local library corresponding to $lib")

        } else {
          Prompt.warn(s"Did not find any library corresponding to $lib")
        }
      } else {
        Prompt.displayResults(res, last, url)
      }
    }
  }
}

// fsToPath is not symmetric with pathTofs
case class ExecUnregistered(script: String, args: List[String]) extends Command {
  override def exec(): Unit = {
    val fs = Helper.pathToFs(script)

    fs.extension match {
      case Some(Script.extension) =>
        if (fs.exists) {
          if (fs.readable) {
            val deps = Script.extractDependencies(fs)
            deps.foreach(_.existOrFetch())
            val classPath = Coursier.cp(deps)
            //Prompt.info(s"Executes the script ${Helper.fsToPath(fs)}")

            val execDir = "/" + Helper.getCurrentDirectory
            Cli.exe(execDir, Seq("scala", "-cp", classPath, Helper.fsToPath(fs)), args)

          } else {
            Prompt.error(s"The script $fs is not readable")
          }
        } else {
          Prompt.error(s"The script $fs does not exist")
        }
      case _ =>
        Prompt.error(s"The extension of $fs is not correct. It should be ${Script.extension}")
    }
  }
}

case class ExecRegistered(alias: String, args: List[String]) extends Command {
  override def exec(): Unit = Script.Listing.get(alias) match {
    case None => Prompt.error(s"The script alias $alias does not exist")
    case Some(rf) =>
      if (rf.path.exists) {
        if (rf.checkSha1()) {
          val deps = Script.extractDependencies(rf.path)
          val dir = Script.Listing.pathToClassesDir(alias)
          val classPath = rf.classpath + ":" + dir

          //Prompt.info(s"Execute the script alias $alias")
          Cli.exe("/" + Helper.getCurrentDirectory, Seq("scala", "-cp", classPath, rf.main), args)

        } else {
          Script.Listing.reCompile(alias, rf.path)
          Prompt.warn(s"The content of the script alias $alias changed. The script has been recompiled. Re-run the command")
        }
      } else {
        Prompt.error(s"the file corresponding to the alias $alias does not exist anymore at ${rf.path}")
      }
  }
}

// TODO IMPERATIF template -> no dep but displays message to use Add
// TODO allow no dep et no dep block

case class AliasAdd(alias: String, script: String, main: Option[String]) extends Command {
  override def exec(): Unit = {
    if (Script.Listing.exists(alias)) {
      Prompt.error(s"The script alias $alias is already used")

    } else {
      val fs = Helper.pathToFs(script)

      fs.extension match {
        case Some(Script.extension) =>
          if (fs.exists) {
            if (fs.readable) {

              main match {
                case None => Script.Listing.add(alias, fs, None, None, "Main")
                case Some(m) => Script.Listing.add(alias, fs, None, None, m)
              }

              if (Script.Listing.exists(alias)) Prompt.info(s"Registered $fs as $alias")

            } else {
              Prompt.error(s"The file $fs is not readable")
            }
          } else {
            Prompt.error(s"The file $fs does not exist")
          }
        case _ =>
          Prompt.error(s"The extension of $fs is not correct. It should be ${Script.extension}")
      }
    }
  }
}

case class AliasDelete(alias: String) extends Command {
  override def exec(): Unit = {
    if (Script.Listing.exists(alias)) {
      Script.Listing.remove(alias)
      Prompt.info(s"The script alias $alias has been deleted")

    } else {
      Prompt.error(s"The script alias $alias does not exist")
    }
  }
}

case class AliasList() extends Command {
  override def exec(): Unit = Script.Listing.all().foreach {
    case (alias, rFile) =>
      val file = rFile.path
      val exists = if (file.exists) "(exists)" else "(missing)"
      val readable = if (file.readable) "(readable)" else "(not readable)"
      val source = rFile.source.getOrElse("local") + rFile.sourcePath.getOrElse("")
      val main = rFile.main
      Prompt.display(s"$alias - $exists $readable from $source, main object $main --> $file")
  }
}

case class AliasChangeMain(alias: String, main: String) extends Command {
  override def exec(): Unit = Script.Listing.get(alias) match {
    case Some(rf) =>
      Script.Listing.updateMain(alias, rf, main)
      Prompt.info(s"The main object of the script alias $alias has been replaced by $main")
    case None => Prompt.error(s"The script alias $alias does not exist")
  }
}

case class AliasUpdate(alias: Option[String]) extends Command {
  override def exec(): Unit = alias match {
    case Some(a) => Script.Listing.get(a) match {
      case Some(rf) => Script.Listing.update(a, rf)
      case None => Prompt.error(s"The script alias $alias does not exist")
    }
    case None => Script.Listing.updateAll()
  }
}

case class AliasRename(alias: String, newAlias: String) extends Command {
  override def exec(): Unit = {
    if (Script.Listing.exists(newAlias)) {
      Prompt.error(s"The new alias $alias is already used")

    } else {
      Script.Listing.get(alias) match {
        case Some(rf) =>
          // could check if script has been change since last compile
          // but since add is carried out before remove it would fail on compile beforehand
          Script.Listing.add(newAlias, rf.path, rf.source, rf.sourcePath, rf.main)
          Script.Listing.remove(alias)
          Prompt.info(s"The script alias $alias has been renamed $newAlias")

        case  None =>
          Prompt.error(s"The script alias $alias does not exist")
      }
    }
  }
}

case class ScriptTemplate(script: String, deps: List[Dependency]) extends Command {
  override def exec(): Unit = {
    val fs = Helper.pathToFs(script)
    val parent = Helper.fsParent(fs)

    if (parent.writable) {
      if (fs.extension == Some("sbk")) {

        Script.template(fs, deps)
        Prompt.info(s"A template has been created at $fs")
      } else {
        Prompt.error(s"Script should have the ${Sbk.toolName} extension. <path>/<scriptName>.${Sbk.toolName}")
      }

    } else {
      Error(s"The directory $parent is not writable")
    }
  }
}

case class ScriptAdd(script: String, dep: Dependency) extends Command {
  override def exec(): Unit = {
    val fs = Helper.pathToFs(script)

    if (fs.exists) {
      if (fs.readable) {
        if (fs.writable) {
          Script.add(List(dep), fs)
          Prompt.info(s"The dependency $dep has been added to the script $script")

        } else {
          Prompt.error(s"The script $fs is not writable")
        }
      } else {
        Prompt.error(s"The script $fs is not readable")
      }
    } else {
      Prompt.error(s"The script $fs does not exist")
    }
  }
}

case class ScriptResolve(script: String) extends Command {
  override def exec(): Unit = {
    val fs = Helper.pathToFs(script)

    if (fs.exists) {
      if (fs.readable) {
        val deps = Script.extractDependencies(fs)
        Coursier.installDependencies(None, deps)

      } else {
        Prompt.error(s"The script $fs is not readable")
      }
    } else {
      Prompt.error(s"The script $fs does not exist")
    }
  }
}

case class PoolAdd(alias: String, pool: Pool) extends Command {
  override def exec(): Unit = {
    if (Github.Listing.exists(alias)) {
      Prompt.error(s"The pool alias $alias is already used")

    } else {
      Github.Listing.add(alias, pool)
      Prompt.info(s"The pool alias $alias has been created")
    }
  }
}

case class PoolRemove(alias: String) extends Command {
  override def exec(): Unit = {
    if (Github.Listing.exists(alias)) {
      Github.Listing.remove(alias)
      Prompt.info(s"The pool alias $alias has been deleted")

    } else {
      Prompt.error(s"The pool alias $alias does not exist")
    }
  }
}

case class PoolList() extends Command {
  override def exec(): Unit = Github.Listing.list()
}

case class PoolToken(alias: String, token: String) extends Command {
  override def exec(): Unit = Github.Listing.get(alias) match {
    case Some(pool) =>
      Github.Listing.remove(alias)
      Github.Listing.add(alias, Pool(pool.org, pool.repo, Some(token)))
      Prompt.info(s"The token ${pool.token} of the pool alias $alias has been changed to $token")

    case None =>
      Prompt.error(s"The pool alias $alias does not exist")
  }
}

case class PoolUpload(scriptAlias: String, poolAlias: Option[String] = None) extends Command {
  override def exec(): Unit = {
    if (Script.Listing.exists(scriptAlias)) {
      val fs = Script.Listing.getPath(scriptAlias).get

      val pool = poolAlias match {
        case Some(p) => Github.Listing.get(p).getOrElse(throw new Exception(s"The pool alias $poolAlias does not exist")) // throw is not consistent with the logic of the function
        case None => Github.getUserPool
      }
      pool.upload(fs)
      Prompt.info(s"Successfully uploaded $scriptAlias to $poolAlias") // TOTO replace with name and could query Github to verify

    } else {
      Prompt.error(s"The script alias $scriptAlias does not exist")
    }
  }
}

case class PoolUpdate(scriptAlias: String, poolAlias: Option[String] = None) extends Command {
  override def exec(): Unit = {
    if (Script.Listing.exists(scriptAlias)) {
      val fs = Script.Listing.getPath(scriptAlias).get

      val pool = poolAlias match {
        case Some(p) => Github.Listing.get(p).getOrElse(throw new Exception(s"The pool alias $poolAlias does not exist")) // throw is not consistent with the logic of the function
        case None => Github.getUserPool
      }
      pool.update(fs)
      Prompt.info(s"Successfully uploaded $scriptAlias to $poolAlias") // TOTO replace with name and could query Github to verify

    } else {
      Prompt.error(s"The script alias $scriptAlias does not exist")
    }
  }
}

// TODO pb Main hardcoded, could be defined in header
case class PoolDownload(name: String, poolAlias: String, scriptAlias: Option[String]) extends Command {
  override def exec(): Unit = {
    Github.Listing.get(poolAlias) match {
      case Some(pool) => pool.download(name) match {
        case None => Error(s"$name is not a file on pool $pool")
        case Some(c) =>
          val dest = pool.destPath(name)
          c.copyTo(dest)
          Prompt.info(s"Successfully downloaded $name at $dest")
          val aliasOrName = scriptAlias.getOrElse(pool.toName + name)
          Script.Listing.add(aliasOrName, dest, Some(pool), Some(name), "Main")
      }
      case None => Prompt.error(s"The pool alias $poolAlias does not exist")
    }
  }
}

case class V() extends Command {
  override def exec(): Unit = Prompt.display(Sbk.toolVersion)
}

case class Error(msg: String) extends Command {
  override def exec(): Unit = Prompt.error(msg)
}

case class H(command: Option[String] = None) extends Command {
  override def exec(): Unit = {
    val str = command match {
      case None => globalHelp()
      case Some(cmd) => help(cmd)
    }

    Prompt.display(str)
  }

  def globalHelp(): String = {
    //|Usage: sbk [--version] [--help] [--noEnhencement] <command> [<args>]
    s"""v0.2 - (c) Apache Licence v2
        |
        |  -n, --no-enhancement   Disable display enhancement
        |  -h, --help             Show help message
        |  -v, --version          Print version
        |
        |Commands:
        |  ${Sbk.toolName} `alias-of-registered-script`   Execute a registered script
        |
        |Subcommand: search - Searches for a library
        |  Usage:
        |    ${Sbk.toolName} search [option] <partial artifactId>
        |    ${Sbk.toolName} search [option] <partial groupId> % <partial artifactId> % <version>*
        |
        |  Options:
        |    -c, --cache   locally only
        |    -l, --last    last version
        |    -u, --url     print also the library url to Scaladex
        |
        |Subcommand: exec - Execute an unregistered script
        |  Usage:
        |    ${Sbk.toolName} exec <path-to-the-script>
        |
        |Subcommand: alias - Registration of scripts (see help alias for more)
        |  Usage:
        |    ${Sbk.toolName} alias [option]
        |
        |  Options:
        |    -a, --add <alias> <path-to-script> <main>*     Register a script
        |    -c, --change-main <alias> <newMain>            Change the Main class to be called when running the script
        |    -d, --delete  <alias>                          Unregister a script
        |    -l, --list                                     List the registered scripts
        |    -r, --rename  <alias> <newAlias>               Change the script alias
        |    -u, --update  <alias>*                         Update one or all scripts obtained from Github
        |
        |Subcommand: script - Various action on scripts
        |  Usage:
        |    ${Sbk.toolName} script [option]
        |
        |  Options:
        |    -r, --resolve  <path-to-script>    Resolve the dependencies declared in a script
        |    -t, --template <destination>       Create a template
        |
        |Subcommand: pool - Registration of Github repositories (pools)
        |  Usage:
        |    ${Sbk.toolName} pool [option]
        |
        |  Options:
        |    -a, --add  <alias> <org> <repo> <token>*                   Register a pool
        |    -r, --remove  <poolAlias>                                  Unregister a pool
        |    -l, --list                                                 List pools
        |    -c, --change-token  <poolAlias> <token>                    Change the oAuth2 token of the pool
        |    -d, --download  <poolAlias> <path-to-file-on-the-repo>     Download a script from a pool
        |    -u, --upload  <scriptAlias> <poolAlias>                    Upload a registered script to the user's pool
       """.stripMargin
  }


  def help(cmd: String): String = cmd match {
    case _ => "use --help"
  }
}
