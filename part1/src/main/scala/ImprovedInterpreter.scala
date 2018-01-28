import Implicits._

/**
  * Created by slee on 2018/1/27.
  */
case class ImprovedInterpreter(expr: String) extends PlusInterpreter {
  var pos = 0

  override def nextToken(): Option[Token] = {
    nextToken(Nil)
  }

  def nextToken(ints: List[Char]): Option[Token] = {
    if (pos > expr.length - 1) {
      if (ints.nonEmpty)
        IntToken(ints.reverse.mkString.toInt)
      else {
        pos += 1
        EOFToken
      }
    } else {
      expr(pos) match {
        case c if c.isDigit => pos += 1; nextToken(c :: ints)
        case _ if ints.nonEmpty =>
          IntToken(ints.reverse.mkString.toInt)
        case '+' =>
          pos += 1
          PlusToken
        case _ =>
          pos += 1
          None
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
}
