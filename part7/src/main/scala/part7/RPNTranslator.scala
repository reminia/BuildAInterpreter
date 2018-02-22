package part7

/**
  * Created by slee on 2018/2/22.
  */
object RPNTranslator {

  def translate(expr: String): String = {
    val ast = ImprovedInterpreter(expr).parse()
    traverse(ast)
  }

  def traverse(ast: AST): String = ast match {
    case Number(v) => s"$v"
    case BinOp(left, right, op) =>
      val operator = op match {
        case Plus => "+"
        case Minus => "-"
        case Multiplication => "*"
        case Divide => "/"
      }
      s"${traverse(left)} ${traverse(right)} $operator"
  }

  def main(args: Array[String]) {
    logger(translate("1 + 2 * 3"))
    logger(translate("(1+2)/3 * 4 + 5"))
  }
}
