package part8

/**
  * Created by slee on 2018/1/27.
  */
case class ImprovedInterpreter(expr: String) extends Interpreter {
  var pos = 0

  var current: Option[Token] = nextToken()

  def exp(t: AST): AST = {
    val result = current match {
      case Some(Plus) => current = nextToken(); Some(BinOp(t, term(factor()), Plus))
      case Some(Minus) => current = nextToken(); Some(BinOp(t, term(factor()), Minus))
      case _ => None
    }
    result.fold(t)(exp(_))
  }

  def term(a: AST): AST = {
    val result = current match {
      case Some(Multiplication) => current = nextToken(); Some(BinOp(a, factor(), Multiplication))
      case Some(Divide) => current = nextToken(); Some(BinOp(a, factor(), Divide))
      case _ => None
    }
    result.fold(a)(term(_))
  }

  def factor(): AST = current match {
    case Some(Plus) => current = nextToken(); UnaryOp(Plus, factor())
    case Some(Minus) => current = nextToken(); UnaryOp(Minus, factor())
    case Some(IntToken(v)) => current = nextToken(); Number(v)
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
    visit(parse())
  }

  def parse(): AST = exp(term(factor()))

  def visit(ast: AST): Int = ast match {
    case Number(v) => v
    case UnaryOp(Minus, v) => -visit(v)
    case UnaryOp(Plus, v) => visit(v)
    case BinOp(a, b, op) => op(visit(a), visit(b))
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
  logger(ImprovedInterpreter("1+2").parse())
  logger(ImprovedInterpreter("1+2").interpret())
  logger(ImprovedInterpreter("1+2*3").parse())
  logger(ImprovedInterpreter("1+2*3").interpret())
  logger(ImprovedInterpreter("1*(2+3)+4/5").parse())
  logger(ImprovedInterpreter("1*(2+3)+4/5").interpret())
  logger(ImprovedInterpreter(" 1 * (2+3) + 8 / 5 ").parse())
  logger(ImprovedInterpreter(" 1 * (2+3) + 8 / 5 ").interpret())
  logger(ImprovedInterpreter(" -1 * (2+3) + 8 / 5 ").parse())
  logger(ImprovedInterpreter(" -1 * (2+3) + 8 / 5 ").interpret())
  logger(ImprovedInterpreter("--1").parse())
  logger(ImprovedInterpreter("--1").interpret())
  logger(ImprovedInterpreter("-(1+2)").parse())
  logger(ImprovedInterpreter("-(1+2)").interpret())
  logger(ImprovedInterpreter("--+3").parse())
  logger(ImprovedInterpreter("--+3").interpret())

}
