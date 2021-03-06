package part2

/**
  * Created by slee on 2018/1/27.
  */
case class ImprovedInterpreter(expr: String) extends Interpreter {
  var pos = 0

  var current = nextToken().flatMap(_.get[IntToken])

  override def interpret(): Option[Int] = {
    while (pos < expr.length && current.nonEmpty) {
      val current1: Option[IntToken] = for {
        left <- current
        op <- nextToken().flatMap(_.get[Operator])
        right <- nextToken().flatMap(_.get[IntToken])
      } yield {
        op(left.value, right.value)
      }
      current = current1
    }
    current.map(_.value)
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

  println(ImprovedInterpreter("2+3 ").interpret()) // cant deal with ending blanks
  println(ImprovedInterpreter("12+3").interpret())
  println(ImprovedInterpreter("12+23").interpret())
  println(ImprovedInterpreter("1234+23").interpret())
  println(ImprovedInterpreter("1234 + 23").interpret())
  println(ImprovedInterpreter(" 1234    + 23  ").interpret())
  println(ImprovedInterpreter("3-2").interpret())
  println(ImprovedInterpreter("3*2").interpret())
  println(ImprovedInterpreter("3 / 2").interpret())

  //test a - b + c ...
  println(ImprovedInterpreter("23 - 3 + 4 - 5").interpret())
  println(ImprovedInterpreter(" 23 - 3 + 4 - 5").interpret())
  println(ImprovedInterpreter("2 + 3 --4").interpret())
}
