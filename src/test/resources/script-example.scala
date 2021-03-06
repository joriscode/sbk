/***
"com.lihaoyi" % "fastparse_2.11" % "0.3.7"
"com.lihaoyi" % "sourcecode_2.11" % "0.1.1"
*/

import fastparse.all._

object HelloWorld extends App {
  println("Hello, world!")

  args.foreach(println)

  def eval(tree: (Int, Seq[(String, Int)])): Int = {
    val (base, ops) = tree
    ops.foldLeft(base) {
      case (left, (op, right)) => op match{
        case "+" => left + right
        case "-" => left - right
        case "*" => left * right
        case "/" => left / right
      }
    }
  }

  val number: P[Int] = P( CharIn('0'to'9').rep(1).!.map(_.toInt) )
  val parens: P[Int] = P( "(" ~/ addSub ~ ")" )
  val factor: P[Int] = P( number | parens )

  val divMul: P[Int] = P( factor ~ (CharIn("*/").! ~/ factor).rep ).map(eval)
  val addSub: P[Int] = P( divMul ~ (CharIn("+-").! ~/ divMul).rep ).map(eval)
  val expr: P[Int]   = P( addSub ~ End )
}

HelloWorld.main(args)
