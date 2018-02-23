package part8

/**
  * Created by slee on 2018/2/22.
  */
object LispTranslator {
  def translate(expr: String): String = traverse(ImprovedInterpreter(expr).parse())

  def traverse(ast: AST): String = ast match {
    case Number(v) => s"$v"
    case BinOp(a, b, op) =>
      val operator = op match {
        case Plus => "+"
        case Minus => "-"
        case Multiplication => "*"
        case Divide => "/"
      }
      s"($operator ${traverse(a)} ${traverse(b)})"
  }

  def main(args: Array[String]) {
    logger(translate("1 + 2 * 3"))
    logger(translate("(1+2)/3 * 4 + 5"))
  }
}
