package part4

/**
  * Created by slee on 2018/1/27.
  */
case class SimpleInterpreter(expr: String) extends Interpreter {
  var pos: Int = 0

  override def nextToken(): Option[Token] = {
    val z: Option[Token] = {
      if (pos > expr.length - 1) {
        EOFToken
      } else {
        expr(pos) match {
          case c if c.isDigit => IntToken(c.toString.toInt)
          case '+' => Plus
          case '-' => Minus
          case _ => None
        }
      }
    }
    pos = pos + 1
    z
  }

}

object TestInterpreter extends App {
  println(SimpleInterpreter("a1a+3").interpret())
  println(SimpleInterpreter("1+2").interpret())
  println(SimpleInterpreter("1+34").interpret()) //non recognize long ints
  println(SimpleInterpreter("1 +34").interpret()) //non recognize blank
  println(SimpleInterpreter("1-2").interpret()) //recognize -
}

