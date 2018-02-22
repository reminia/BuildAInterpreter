package part5


/**
  * Created by slee on 2018/1/27.
  */
case class SimpleInterpreter(expr: String) extends Interpreter {
  var pos: Int = 0

  def integer(token: Option[Token]): Int = token match {
    case Some(IntToken(v)) => v
    case _ => throw new Exception("a token(int) was expected")
  }

  def op(token: Option[Token]): Operator = token match {
    case Some(Plus) => Plus
    case Some(Minus) => Minus
    case _ => throw new Exception("a operator(+ or -) was expected")
  }

  def interpret(): Int = {
    val left = integer(nextToken())
    val opr = op(nextToken())
    val right = integer(nextToken())
    opr(left, right)
  }

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
  logger(SimpleInterpreter("a1a+3").interpret())
  logger(SimpleInterpreter("1+2").interpret())
  logger(SimpleInterpreter("1+34").interpret()) //non recognize long ints
  logger(SimpleInterpreter("1 +34").interpret()) //non recognize blank
  logger(SimpleInterpreter("1-2").interpret()) //recognize -
}

