package part6

/**
  * Created by slee on 2018/1/27.
  */
case class ImprovedInterpreter(expr: String) extends Interpreter {
  var pos = 0

  var current: Option[Token] = nextToken()

  def exp(t: IntToken): IntToken = {
    val result = current match {
      case Some(Plus) => current = nextToken(); Some(Plus(t, term(factor())))
      case Some(Minus) => current = nextToken(); Some(Minus(t, term(factor())))
      case _ => None
    }
    result.fold(t)(exp(_))
  }

  def term(a: IntToken): IntToken = {
    val result = current match {
      case Some(Multiplication) => current = nextToken(); Some(Multiplication(a, factor()))
      case Some(Divide) => current = nextToken(); Some(Divide(a, factor()))
      case _ => None
    }
    result.fold(a)(term(_))
  }

  def factor(): IntToken = current match {
    case Some(a@IntToken(v)) => current = nextToken(); a
    case Some(LeftPara) => leftPara(); val res = exp(term(factor())); rightPara(); res
    case _ => throw new Exception("a factor(int) was expected")
  }

  def leftPara(): Unit = current match {
    case Some(LeftPara) => current = nextToken()
    case _ => throw new Exception("a left ( was expected")
  }

  def rightPara(): Unit = current match {
    case Some(RightPara) => current = nextToken()
    case _ => throw new Exception("a right ) was expected")
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
        case '(' => pos += 1; LeftPara
        case ')' => pos += 1; RightPara
        case _ => pos += 1; None
      }
    }
  }
}

object TestImprovedInterpreter extends App {

  logger(part6.ImprovedInterpreter("2^^3").interpret())
  logger(part6.ImprovedInterpreter("2+3 ").interpret())
  logger(part6.ImprovedInterpreter("12+3").interpret())
  logger(part6.ImprovedInterpreter("12+23").interpret())
  logger(part6.ImprovedInterpreter("1234+23").interpret())
  logger(part6.ImprovedInterpreter("1234 + 23").interpret())
  logger(part6.ImprovedInterpreter(" 1234    + 23  ").interpret())
  logger(part6.ImprovedInterpreter("3-2").interpret())
  logger(part6.ImprovedInterpreter("3*2").interpret())
  logger(part6.ImprovedInterpreter("3 / 2").interpret())
  //test a - b + c ...

  logger(part6.ImprovedInterpreter("23 - 3 + 4 - 5").interpret())
  logger(part6.ImprovedInterpreter(" 23 - 3 + 4 - 5").interpret())
  logger(part6.ImprovedInterpreter("2 + 3 --4").interpret())
  logger(part6.ImprovedInterpreter("2 + 3 --").interpret())
  logger(part6.ImprovedInterpreter("3*5/6").interpret())
  logger(part6.ImprovedInterpreter("3*5+2").interpret())
  logger(part6.ImprovedInterpreter("3-5*2").interpret())
  logger(part6.ImprovedInterpreter("3/5*2-3*1").interpret())
  logger(part6.ImprovedInterpreter("3+5-2-3").interpret())

  // test parentheses
  logger(part6.ImprovedInterpreter("(1+2)*3").interpret())
  logger(part6.ImprovedInterpreter("(1+2)*(3+4)").interpret())
  logger(part6.ImprovedInterpreter("(1+2)*(3+4-5)-1*2").interpret())
  logger(part6.ImprovedInterpreter("4*(1-2)/3").interpret())

}
