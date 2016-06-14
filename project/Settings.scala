import sbt._

/**
  * Application settings. Configure the build for your application here.
  * You normally don't have to touch the actual build definition after this.
  */
object Settings {
  /** Options for the scala compiler */
  val scalacOptions = Seq(
    "-Xfatal-warnings",
    "-Xlint",
    "-Ybackend:GenBCode",
    "-Ydelambdafy:method",
    "-Yinline-warnings",
    "-Yno-adapted-args",
    "-Yrangepos",
    "-Ywarn-dead-code",
    "-Ywarn-numeric-widen",
    "-Ywarn-unused-import",
    //"-Ywarn-value-discard",
    "-deprecation",
    "-encoding", "UTF-8",
    "-feature",
    "-unchecked"
  )

  /** Declare global dependency versions here to avoid mismatches in multi part dependencies */
  object versions {
    val coursier = "1.0.0-M11"
    val coursierCache = "1.0.0-M11"
    val fastParse = "0.3.7"
    val hasher = "1.2.0"
    val rapture = "2.0.0-M6"
    val scala = "2.11.8"
    val scalatest = "2.2.6"
    val scallop = "1.0.1"
  }

  /** Dependencies only used by the JVM project */
  val dependencies = Def.setting(Seq(
      "com.lihaoyi" %% "fastparse" % versions.fastParse,
      "com.propensive" %% "rapture-fs" % versions.rapture,
      "com.propensive" %% "rapture-html" % versions.rapture,
      "com.propensive" %% "rapture-http-jetty" % versions.rapture,
      "com.propensive" %% "rapture-http-json" % versions.rapture,
      "com.propensive" %% "rapture-json-jawn" % versions.rapture,
      "com.propensive" %% "rapture-net" % versions.rapture,
      "com.roundeights" %% "hasher" % versions.hasher,
      "io.get-coursier" %% "coursier" % versions.coursier,
      "io.get-coursier" %% "coursier-cache" % versions.coursierCache,
      "org.rogach" %% "scallop" % versions.scallop,
      "org.scalatest" % "scalatest_2.11" % versions.scalatest % "test"
  ))
}
