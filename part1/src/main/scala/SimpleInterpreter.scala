import Implicits._

/**
  * Created by slee on 2018/1/27.
  */
case class SimpleInterpreter(expr: String) extends PlusInterpreter {
  var pos: Int = 0

  def nextToken(): Option[Token] = {
    val z: Option[Token] = {
      if (pos > expr.length - 1) {
        EOFToken
      } else {
        expr(pos) match {
          case c if c.isDigit => IntToken(c.toString.toInt)
          case '+' => PlusToken
          case _ => None
        }
      }
    }
    pos = pos + 1
    z
  }

}

object TestInterpreter extends App {
  println(SimpleInterpreter("1+3").interpret())
  println(SimpleInterpreter("1+2").interpret())
  println(SimpleInterpreter("1+34").interpret()) //non recognize long ints
  println(SimpleInterpreter("1-2").interpret()) //non recognize -
  println(SimpleInterpreter("1 +34").interpret()) //non recognize blank
}

