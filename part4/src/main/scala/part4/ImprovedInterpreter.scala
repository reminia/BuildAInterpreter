package part4

/**
  * Created by slee on 2018/1/27.
  */
case class ImprovedInterpreter(expr: String) extends Interpreter {
  var pos = 0

  var current: Option[Token] = None

  def exp(t: Option[IntToken]): Option[IntToken] = {
    val plus = current.flatMap(_.get[Plus.type])
    val minus = current.flatMap(_.get[Minus.type])
    if (plus.nonEmpty || minus.nonEmpty) {
      val res: Option[IntToken] = for {
        a <- t
        op <- current.flatMap(_.get[Operator])
        b <- term(factor())
      } yield {
        op(a.value, b.value)
      }
      exp(res)
    } else {
      t
    }
  }

  def term(fact: Option[IntToken]): Option[IntToken] = {
    current = nextToken()
    val mult = current.flatMap(_.get[Multiplication.type])
    val divide = current.flatMap(_.get[Divide.type])
    if (mult.nonEmpty || divide.nonEmpty) {
      val res: Option[IntToken] = for {
        a <- fact
        op <- current.flatMap(_.get[Operator])
        b <- factor()
      } yield {
        op(a.value, b.value)
      }
      term(res)
    } else {
      fact
    }
  }

  def factor() = nextToken().flatMap(_.get[IntToken])

  override def interpret(): Option[Int] = {
    exp(term(factor())).map(_.value)
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

  println(part4.ImprovedInterpreter("2+3 ").interpret())
  println(part4.ImprovedInterpreter("12+3").interpret())
  println(part4.ImprovedInterpreter("12+23").interpret())
  println(part4.ImprovedInterpreter("1234+23").interpret())
  println(part4.ImprovedInterpreter("1234 + 23").interpret())
  println(part4.ImprovedInterpreter(" 1234    + 23  ").interpret())
  println(part4.ImprovedInterpreter("3-2").interpret())
  println(part4.ImprovedInterpreter("3*2").interpret())
  println(part4.ImprovedInterpreter("3 / 2").interpret())

  //test a - b + c ...
  println(part4.ImprovedInterpreter("23 - 3 + 4 - 5").interpret())
  println(part4.ImprovedInterpreter(" 23 - 3 + 4 - 5").interpret())
  println(part4.ImprovedInterpreter("2 + 3 --4").interpret())
  println(part4.ImprovedInterpreter("2 + 3 --").interpret())
  println(part4.ImprovedInterpreter("3*5/6").interpret())
  println(part4.ImprovedInterpreter("3*5+2").interpret())
  println(part4.ImprovedInterpreter("3-5*2").interpret())
  println(part4.ImprovedInterpreter("3/5*2-3*1").interpret())
  println(part4.ImprovedInterpreter("3+5-2-3").interpret())

}
