package center.scala.sbk

import collection.mutable.Stack
import org.scalatest._

class ExampleSpec extends FlatSpec with Matchers {
  //val lib1 = MavenLibrary()
  //val lib2 = MavenLibrary()
  //val lib3 = MavenLibrary()

  //val lib = MavenLibrary("com.typesafe.play", "play-json_2.11", "2.5.1", None)
  //val lib2 = MavenLibrary("com.typesafe.play", "play-json_2.11", "2.5.2", None)
  //val lib3 = MavenLibrary("com.typesafe.play", "ttu.11", "2.5.2", None)


  "Map" should "contain" in {
    //map.store(lib1)
    //stack.push(2)
    //stack.pop() should be (2)
    //stack.pop() should be (1)
  }

  it should "throw NoSuchElementException if an empty stack is popped" in {
    val emptyStack = new Stack[Int]
    a [NoSuchElementException] should be thrownBy {
      emptyStack.pop()
    } 
  }
}
//val test = Script.exec(SBK.scriptTest)
//val test = Script.parse(SBK.scriptTest)
//Script.template(List(lib3), "/Users/macarty/Desktop/toto.txt")

// TEST MAP
//val map = new SBK.Mapping
//map.store(lib)
//map.store(lib2)
//map.store(lib3)
//map.getLib("json").foreach(println(_))

/* Test FsUrl conversion */
//println(Script.pathToFs("./toto"))
//println(Script.pathToFs("../toto"))
//println(Script.pathToFs("~/toto"))
//println(Script.pathToFs("/toto"))
//println(Script.pathToFs("toto"))

/* Test TempLib.fromString */
//println(TempLib.fromString("rapture".split(" ").toList))
//println(TempLib.fromString("com.typesafe.play % play-json % 2.5.2".split(" ").toList))
//println(TempLib.fromString("com.typesafe.play %% play-json % 2.5.2".split(" ").toList))
//println(TempLib.fromString("com.typesafe.play % play-json_2.11 % 2.5.2".split(" ").toList))
//println(TempLib.fromString("com.typesafe.play % play-json % 2.5.x".split(" ").toList))
//println(TempLib.fromString("com.typesafe.play % play-json % \"\"".split(" ").toList))
//println(TempLib.fromString("com.typesafe.play % play-json".split(" ").toList))
