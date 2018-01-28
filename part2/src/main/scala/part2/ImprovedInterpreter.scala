package part2

/**
  * Created by slee on 2018/1/27.
  */
case class ImprovedInterpreter(expr: String) extends Interpreter {
  var pos = 0

  def nextToken(): Option[Token] = {
    if (pos > expr.length - 1) {
      EOFToken
    } else {
      val number = """\d+""".r
      expr(pos) match {
        case c if c.isDigit => {
          val ints = number.findFirstIn(expr.substring(pos)).get
          pos += ints.length
          IntToken(ints.toInt)
        }
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

  println(ImprovedInterpreter("2+3").interpret())
  println(ImprovedInterpreter("12+3").interpret())
  println(ImprovedInterpreter("12+23").interpret())
  println(ImprovedInterpreter("1234+23").interpret())
  println(ImprovedInterpreter("1234 + 23").interpret())
  println(ImprovedInterpreter(" 1234    + 23  ").interpret())
  println(ImprovedInterpreter(" 23 + 2 3  ").interpret())
  println(ImprovedInterpreter("12 34 + 23").interpret())

  //test a-b
  println(ImprovedInterpreter(" 23 - 3").interpret())
  println(ImprovedInterpreter(" 23 - 34").interpret())
  println(ImprovedInterpreter("3-2").interpret())
  println(ImprovedInterpreter("3*2").interpret())
  println(ImprovedInterpreter("3 / 2").interpret())

}
