import Implicits._

/**
  * Created by slee on 2018/1/27.
  */
case class Interpreter(expr: String) {
  var pos: Int = 0

  def interpret(): Option[Int] = {
    for {
      left <- nextToken().flatMap(_.get[IntToken])
      plus <- nextToken().flatMap(_.get[PlusToken.type])
      right <- nextToken().flatMap(_.get[IntToken])
    } yield {
      left.value + right.value
    }
  }

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
  println(Interpreter("1+3").interpret())
  println(Interpreter("1+2").interpret())
  println(Interpreter("1+34").interpret()) //non recognize long ints
  println(Interpreter("1-2").interpret()) //non recognize -
  println(Interpreter("1 +34").interpret()) //non recognize blank
}

