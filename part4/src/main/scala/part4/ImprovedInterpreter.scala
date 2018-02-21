package part4

/**
  * Created by slee on 2018/1/27.
  */
case class ImprovedInterpreter(expr: String) extends Interpreter {
  var pos = 0

  var op: Option[Token] = None

  def exp(t: IntToken): IntToken = {
    val result = op match {
      case Some(Plus) => Some(Plus(t, term(factor())))
      case Some(Minus) => Some(Minus(t, term(factor())))
      case _ => None
    }
    result.fold(t)(exp(_))
  }

  def term(a: IntToken): IntToken = {
    op = nextToken()
    val result = op match {
      case Some(Multiplication) => Some(Multiplication(a, factor()))
      case Some(Divide) => Some(Divide(a, factor()))
      case _ => None
    }
    result.fold(a)(term(_))
  }

  def factor(): IntToken = nextToken() match {
    case Some(a@IntToken(v)) => a
    case _ => throw new Exception("a factor(int) was expected")
  }

  override def interpret(): Int = {
    exp(term(factor())).value
  }

  def nextToken(): Option[Token] = {
    val number = """\d+""".r
    if (pos > expr.length - 1) None
    else {
      expr(pos) match {
        case c if c.isDigit =>
          val ints = number.findFirstIn(expr.substring(pos)).get
          pos += ints.length
          IntToken(ints.toInt)
        case ' ' => pos += 1; nextToken()
        case '+' => pos += 1; Plus
        case '-' => pos += 1; Minus
        case '*' => pos += 1; Multiplication
        case '/' => pos += 1; Divide
        case _ => pos += 1; None
      }
    }
  }
}

object TestImprovedInterpreter extends App {

  logger(part4.ImprovedInterpreter("2+3 ").interpret())
  logger(part4.ImprovedInterpreter("12+3").interpret())
  logger(part4.ImprovedInterpreter("12+23").interpret())
  logger(part4.ImprovedInterpreter("1234+23").interpret())
  logger(part4.ImprovedInterpreter("1234 + 23").interpret())
  logger(part4.ImprovedInterpreter(" 1234    + 23  ").interpret())
  logger(part4.ImprovedInterpreter("3-2").interpret())
  logger(part4.ImprovedInterpreter("3*2").interpret())
  logger(part4.ImprovedInterpreter("3 / 2").interpret())
  //test a - b + c ...

  logger(part4.ImprovedInterpreter("23 - 3 + 4 - 5").interpret())
  logger(part4.ImprovedInterpreter(" 23 - 3 + 4 - 5").interpret())
  logger(part4.ImprovedInterpreter("2 + 3 --4").interpret())
  logger(part4.ImprovedInterpreter("2 + 3 --").interpret())
  logger(part4.ImprovedInterpreter("3*5/6").interpret())
  logger(part4.ImprovedInterpreter("3*5+2").interpret())
  logger(part4.ImprovedInterpreter("3-5*2").interpret())
  logger(part4.ImprovedInterpreter("3/5*2-3*1").interpret())
  logger(part4.ImprovedInterpreter("3+5-2-3").interpret())

}
