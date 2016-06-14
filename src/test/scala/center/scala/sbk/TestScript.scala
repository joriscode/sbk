package center.scala.sbk

import org.scalatest._
import rapture.fs._
import rapture.io._
import rapture.codec._
import encodings.`UTF-8`._

class TestScript extends FlatSpec with Matchers {
  val file = getClass.getResource("/script-test.scala").getFile
  println(file)

  val fs = FsUrl(file.split("/"))
  println(fs)

  "script" should "parsed" in {
    val expectedDeps = List(
      Dependency("com.lihaoyi", "fastparse_2.11", "0.3.7"),
      Dependency("com.typesafe.play", "play-json_2.11", "2.5.1")
    )

    val expectedBody = "\nobject HelloWorld {\n  def main(args: Array[String]): Unit = {\n    println(\"Hello, world!\")\n  }\n}\n"

    // TODO test header
    val (header, deps, body) = Script.fastParse(fs.slurp[Char])

    deps should be (expectedDeps)
    body should be (expectedBody)
  }
}
// TODO write tests!
// strt with
//val scriptTest = SBK.sbk / "script.sh"
//Script.extractDependencies(scriptTest)
