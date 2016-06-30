package center.scala.sbk

import center.scala.sbk.Github.Pool
import org.rogach.scallop._
import org.rogach.scallop.exceptions._

import scala.language.reflectiveCalls

object Scallop {

  class Conf(args: Seq[String]) extends ScallopConf(args) {
    //version(s"\nv${Sbk.toolVersion} - (c) Apache Licence v2")
    banner(s"""
              |Usage: ${Sbk.toolName} [--version] [--help] [--noEnhencement] <command> [<args>]
              |""".stripMargin)

    //footer("")
    //shortSubcommandsHelp(true)

    // global options
    val version = opt[Boolean](hidden=true, descr = "Print version")
    val noEnhancement = opt[Boolean](hidden=false, descr = "Disable display enhancement")

    val search = new Subcommand("search") {
      descr("Searches for a library")
      val cache = opt[Boolean](descr = "locally only")
      val last = opt[Boolean](descr = "last version")
      val url = opt[Boolean](descr = "print also the library url to Scaladex")
      val lib = trailArg[InputLibrary]()

      val s = for {
        li <- lib
        c <- cache
        la <- last
        u <- url
      } yield Search(li, c, la, u)
    }

    val exec = new Subcommand("exec") {
      descr("Execute an unregistered script")
      val script = trailArg[ExecUnregistered](descr = "path to the script")
    }

    val alias = new Subcommand("alias") {
      descr("Registration of scripts (see help alias for more)")
      //banner("banner")
      val add = opt[AliasAdd](descr = "register a script: -a alias script")
      val delete = opt[AliasDelete](descr = "unregister a script: -d alias")
      val list = opt[Boolean](descr = "list the registered scripts: -l")
      val changeMain = opt[AliasChangeMain](descr = "change the Main class to be called when running the script: -c alias newMain")
      val update = opt[AliasUpdate](descr = "update one or all scripts obtained from Github: -u alias (optional)")
      val rename = opt[AliasRename](descr = "change the script alias: -r alias newAlias")
      requireOne(add, delete, list, changeMain, update, rename)
      mutuallyExclusive(add, delete, list, changeMain, update, rename)
    }

    val script = new Subcommand("script") {
      descr("Various action on scripts")
      val template = opt[ScriptTemplate](descr = "create a template: -t destination")
      //val addLibrary = opt[ScriptAdd](descr = "add a library to a script: -a path") // TODO
      val resolve = opt[ScriptResolve](descr = "resolve the dependencies declared in a script: -r path")
      requireOne(template, resolve)
      mutuallyExclusive(template, resolve)
    }

    val pool = new Subcommand("pool") {
      descr("Registration of Github pools")
      val add = opt[PoolAdd](descr = "Register a pool: -a alias org repo token (optional)")
      val remove = opt[PoolRemove](descr = "Unregister a pool: -d alias")
      val changeToken = opt[PoolToken](descr = "Change the oAuth2 token of the pool: -c alias newToken")
      val upload = opt[PoolUpload](descr = "Upload a registered script to the user's pool: -u scriptAlias poolAlias")
      val update = opt[PoolUpdate](descr = "Update a registered script to the user's pool: -u scriptAlias poolAlias")
      val download = opt[PoolDownload](descr = "Download a script from a pool: -d poolAlias file")
      requireOne(add, remove, changeToken, upload, download)
      mutuallyExclusive(add, remove, changeToken, upload, download)
    }

    /* overwrite help */
    val help = opt[Boolean](descr = "Help")

    addSubcommand(search)
    addSubcommand(exec)
    addSubcommand(alias)
    addSubcommand(script)
    addSubcommand(pool)

    val shortCut = trailArg[ExecRegistered](descr = s"Execute a registered script. Usage: ${Sbk.toolName} <alias>", required = false)

    override def onError(e: Throwable): Unit = e match {
      case Help("") => H()
      case Help(subcommandName) => H(Some(subcommandName))
      case Version =>
      case Exit() => // catches both Help and Error
      case ScallopException(message) => // catches all excepitons
      case RequiredOptionNotFound(optionName) =>
      // you can also conveniently match on exceptions
      case other => throw other
    }

    verify()
  }


  implicit val aliasAddConverter = new ValueConverter[AliasAdd] {
    def parse(s: List[(String, List[String])]): Either[String, Option[AliasAdd]] = {
      s match {
        case (_, alias :: script :: Nil) :: Nil => Right(Some(AliasAdd(alias, script, None)))
        case (_, alias :: script :: main :: Nil) :: Nil => Right(Some(AliasAdd(alias, script, Some(main))))
        case Nil => Right(None)
        case _ => Left("alias add parser error")
      }
    }

    val tag = scala.reflect.runtime.universe.typeTag[AliasAdd]
    val argType = org.rogach.scallop.ArgType.LIST
  }

  implicit val aliasRemoveConverter = new ValueConverter[AliasDelete] {
    def parse(s: List[(String, List[String])]): Either[String, Option[AliasDelete]] = {
      s match {
        case (_, alias :: Nil) :: Nil => Right(Some(AliasDelete(alias)))
        case Nil => Right(None)
        case _ => Left("alias remove parser error")
      }
    }

    val tag = scala.reflect.runtime.universe.typeTag[AliasDelete]
    val argType = org.rogach.scallop.ArgType.LIST
  }

  implicit val aliasChangeMainConverter = new ValueConverter[AliasChangeMain] {
    def parse(s: List[(String, List[String])]): Either[String, Option[AliasChangeMain]] = {
      s match {
        case (_, alias :: main :: Nil) :: Nil => Right(Some(AliasChangeMain(alias, main)))
        case Nil => Right(None)
        case _ => Left("alias changeMain parser error")
      }
    }

    val tag = scala.reflect.runtime.universe.typeTag[AliasChangeMain]
    val argType = org.rogach.scallop.ArgType.LIST
  }

  implicit val aliasUpdateConverter = new ValueConverter[AliasUpdate] {
    def parse(s: List[(String, List[String])]): Either[String, Option[AliasUpdate]] = {
      s match {
        case (_, Nil) :: Nil => Right(Some(AliasUpdate(None)))
        case (_, alias :: Nil) :: Nil => Right(Some(AliasUpdate(Some(alias))))
        case Nil => Right(None)
        case _ => Left("alias update parser error")
      }
    }

    val tag = scala.reflect.runtime.universe.typeTag[AliasUpdate]
    val argType = org.rogach.scallop.ArgType.LIST
  }

  implicit val aliasRenameConverter = new ValueConverter[AliasRename] {
    def parse(s: List[(String, List[String])]): Either[String, Option[AliasRename]] = {
      s match {
        case (_, alias :: newAlias :: Nil) :: Nil => Right(Some(AliasRename(alias, newAlias)))
        case Nil => Right(None)
        case _ => Left("alias rename parser error")
      }
    }

    val tag = scala.reflect.runtime.universe.typeTag[AliasRename]
    val argType = org.rogach.scallop.ArgType.LIST
  }

  implicit val scriptTemplateConverter = new ValueConverter[ScriptTemplate] {
    def parse(s: List[(String, List[String])]): Either[String, Option[ScriptTemplate]] = {
      s match {
        case (_, script :: Nil) :: Nil => Right(Some(ScriptTemplate(script, Nil))) // TODO no libs
        case Nil => Right(None)
        case _ => Left("script template parser error")
      }
    }

    val tag = scala.reflect.runtime.universe.typeTag[ScriptTemplate]
    val argType = org.rogach.scallop.ArgType.LIST
  }

  // implicit val scriptAddConverter = new ValueConverter[ScriptAdd] {
  //   def parse(s: List[(String, List[String])]): Either[String, Option[ScriptAdd]] = {
  //     s match {
  //       case (_, script :: Nil) :: Nil => Right(Some(ScriptAdd(script, Dependency())))
  //       case Nil => Right(None)
  //       case _ => Left("script add parser error")
  //     }
  //   }

  //   val tag = scala.reflect.runtime.universe.typeTag[ScriptAdd]
  //   val argType = org.rogach.scallop.ArgType.LIST
  // }

  implicit val scriptResolveConverter = new ValueConverter[ScriptResolve] {
    def parse(s: List[(String, List[String])]): Either[String, Option[ScriptResolve]] = {
      s match {
        case (_, script :: Nil) :: Nil => Right(Some(ScriptResolve(script)))
        case Nil => Right(None)
        case _ => Left("script resolve parser error")
      }
    }

    val tag = scala.reflect.runtime.universe.typeTag[ScriptResolve]
    val argType = org.rogach.scallop.ArgType.LIST
  }

  implicit val execUnregisteredConverter = new ValueConverter[ExecUnregistered] {
    def parse(s: List[(String, List[String])]): Either[String, Option[ExecUnregistered]] = {
      s match {
        case (_, script :: xs) :: Nil => Right(Some(ExecUnregistered(script, xs)))
        case Nil => Right(None)
        case _ => Left("exec parser error")
      }
    }

    val tag = scala.reflect.runtime.universe.typeTag[ExecUnregistered]
    val argType = org.rogach.scallop.ArgType.LIST
  }

  /*
  implicit val TOTOConverter = new ValueConverter[TOTO] {
    def parse(s: List[(String, List[String])]): Either[String, Option[TOTO]] = {
      s match {
        case (_, alias :: Nil) :: Nil => Right(Some(TOTO(alias)))
        case Nil => Right(None)
        case _ => Left("Alias Add parser error")
      }
    }

    val tag = scala.reflect.runtime.universe.typeTag[TOTO]
    val argType = org.rogach.scallop.ArgType.LIST
  }
  */

  implicit val poolConverter = new ValueConverter[Pool] {
    def parse(s: List[(String, List[String])]): Either[String, Option[Pool]] = s match {
      case (_, org :: repo :: Nil) :: Nil => Right(Some(Pool(org, repo, None)))
      case (_, org :: repo :: token :: Nil) :: Nil => Right(Some(Pool(org, repo, Some(token))))
      case Nil => Right(None)
      case _ => Left("pool parser error")
    }

    val tag = scala.reflect.runtime.universe.typeTag[Pool]
    val argType = org.rogach.scallop.ArgType.LIST
  }

  implicit val poolAddConverter = new ValueConverter[PoolAdd] {
    def parse(s: List[(String, List[String])]): Either[String, Option[PoolAdd]] = {
      s match {
        //case (_, alias :: xs) :: Nil => Right(Some(PoolAdd(alias, poolConverter.parse())))
        case (_, alias :: org :: repo :: Nil) :: Nil => Right(Some(PoolAdd(alias, Pool(org, repo, None))))
        case (_, alias :: org :: repo :: token :: Nil) :: Nil => Right(Some(PoolAdd(alias, Pool(org, repo, Some(token))))) // TODO should check token
        case Nil => Right(None)
        case _ => Left("pool add parser error")
      }
    }

    val tag = scala.reflect.runtime.universe.typeTag[PoolAdd]
    val argType = org.rogach.scallop.ArgType.LIST
  }

  implicit val poolRemoveConverter = new ValueConverter[PoolRemove] {
    def parse(s: List[(String, List[String])]): Either[String, Option[PoolRemove]] = {
      s match {
        case (_, alias :: Nil) :: Nil => Right(Some(PoolRemove(alias)))
        case Nil => Right(None)
        case _ => Left("pool remove parser error")
      }
    }

    val tag = scala.reflect.runtime.universe.typeTag[PoolRemove]
    val argType = org.rogach.scallop.ArgType.LIST
  }

  implicit val poolTokenConverter = new ValueConverter[PoolToken] {
    def parse(s: List[(String, List[String])]): Either[String, Option[PoolToken]] = {
      s match {
        case (_, alias :: token :: Nil) :: Nil => Right(Some(PoolToken(alias, token)))
        case Nil => Right(None)
        case _ => Left("pool token parser error")
      }
    }

    val tag = scala.reflect.runtime.universe.typeTag[PoolToken]
    val argType = org.rogach.scallop.ArgType.LIST
  }

  implicit val poolUploadConverter = new ValueConverter[PoolUpload] {
    def parse(s: List[(String, List[String])]): Either[String, Option[PoolUpload]] = {
      s match {
        case (_, alias :: Nil) :: Nil => Right(Some(PoolUpload(alias, None)))
        case (_, alias :: poolAlias :: Nil) :: Nil => Right(Some(PoolUpload(alias, Some(poolAlias))))
        case Nil => Right(None)
        case _ => Left("pool upload parser error")
      }
    }

    val tag = scala.reflect.runtime.universe.typeTag[PoolUpload]
    val argType = org.rogach.scallop.ArgType.LIST
  }

  implicit val poolUpdateConverter = new ValueConverter[PoolUpdate] {
    def parse(s: List[(String, List[String])]): Either[String, Option[PoolUpdate]] = {
      s match {
        case (_, alias :: Nil) :: Nil => Right(Some(PoolUpdate(alias, None)))
        case (_, alias :: poolAlias :: Nil) :: Nil => Right(Some(PoolUpdate(alias, Some(poolAlias))))
        case Nil => Right(None)
        case _ => Left("pool update parser error")
      }
    }

    val tag = scala.reflect.runtime.universe.typeTag[PoolUpdate]
    val argType = org.rogach.scallop.ArgType.LIST
  }

  implicit val poolDownloadConverter = new ValueConverter[PoolDownload] {
    def parse(s: List[(String, List[String])]): Either[String, Option[PoolDownload]] = {
      s match {
        case (_, poolAlias :: path :: Nil) :: Nil => Right(Some(PoolDownload(path, poolAlias, None)))
        case (_, poolAlias :: path :: scriptAlias :: Nil) :: Nil => Right(Some(PoolDownload(path, poolAlias, Some(scriptAlias))))
        case Nil => Right(None)
        case _ => Left("pool download parser error")
      }
    }

    val tag = scala.reflect.runtime.universe.typeTag[PoolDownload]
    val argType = org.rogach.scallop.ArgType.LIST
  }

  implicit val inputLibraryConverter = new ValueConverter[InputLibrary] {
    def parse(s: List[(String, List[String])]): Either[String, Option[InputLibrary]] = {
      s match {
        case (_, xs) :: Nil => Right(Some(InputLibrary.fromString(xs)))
        case Nil => Right(None)
        case _ => Left("input library parser error")
      }
    }


    val tag = scala.reflect.runtime.universe.typeTag[InputLibrary]
    val argType = org.rogach.scallop.ArgType.LIST
  }

  implicit val execRegisteredConverter = new ValueConverter[ExecRegistered] {
    def parse(s: List[(String, List[String])]): Either[String, Option[ExecRegistered]] = {
      s match {
        case (_, alias :: args) :: Nil => Right(Some(ExecRegistered(alias, args)))
        case Nil => Right(None)
        case _ => Left("exec registered parser error")
      }
    }

    val tag = scala.reflect.runtime.universe.typeTag[ExecRegistered]
    val argType = org.rogach.scallop.ArgType.LIST
  }

  implicit val installProjectConverter = new ValueConverter[(String, Option[String])] {
    def parse(s: List[(String, List[String])]): Either[String, Option[(String, Option[String])]] = {
      s match {
        case (_, formula :: Nil) :: Nil => Right(Some((formula, None)))
        case (_, org :: repo :: Nil) :: Nil => Right(Some((org, Some(repo))))
        case Nil => Right(None)
        case _ => Left("install project parser error")
      }
    }

    val tag = scala.reflect.runtime.universe.typeTag[(String, Option[String])]
    val argType = org.rogach.scallop.ArgType.LIST
  }


  def whichCommand(args: Seq[String]): Command = {
    val conf = new Conf(args)

    if (conf.noEnhancement.supplied) Prompt.options = Set('r')

    if (conf.shortCut.supplied) {
      conf.shortCut.get.get

    } else if (conf.help.supplied) {
      H()

    } else if (conf.version.supplied) {
      V()

    } else {
      conf.subcommand match {
        case Some(conf.search) =>
          if (conf.search.s.supplied) {
            conf.search.s.get.get
          } else {
            Error(s"This search subcommand does not exist. See --help")
          }

        case Some(conf.exec) =>
          if (conf.exec.script.supplied) {
            conf.exec.script.get.get
          } else {
            Error("This exec subcommand does not exist. See --help")
          }

        case Some(conf.alias) =>
          if (conf.alias.add.supplied) {
            conf.alias.add.get.get

          } else if (conf.alias.delete.supplied) {
            conf.alias.delete.get.get

          } else if (conf.alias.list.supplied) {
            AliasList()

          } else if (conf.alias.changeMain.supplied) {
            conf.alias.changeMain.get.get

          } else if (conf.alias.update.supplied) {
            conf.alias.update.get.get

          } else if (conf.alias.rename.supplied) {
            conf.alias.rename.get.get

          } else {
            Error("This alias subcommand does not exist. See --help")
          }

        case Some(conf.script) =>
          if (conf.script.template.supplied) {
            conf.script.template.get.get

          } else if (conf.script.resolve.supplied) {
            conf.script.resolve.get.get

          } else {
            Error("This script subcommand does not exist. See --help")
          }

        case Some(conf.pool) =>
          if (conf.pool.add.supplied) {
            conf.pool.add.get.get

          } else if (conf.pool.remove.supplied) {
            conf.pool.remove.get.get

          } else if (conf.pool.changeToken.supplied) {
            conf.pool.changeToken.get.get

          } else if (conf.pool.download.supplied) {
            conf.pool.download.get.get

          } else if (conf.pool.upload.supplied) {
            conf.pool.upload.get.get

          } else if (conf.pool.update.supplied) {
            conf.pool.update.get.get

          } else {
            Error("This pool subcommand does not exist. See --help")
          }

        case _ => Error("No command provided. See --help")
      }
    }
  }
}
