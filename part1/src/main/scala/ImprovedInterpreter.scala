/**
  * Created by slee on 2018/1/27.
  */

case class ImprovedInterpreter(expr: String) extends PlusInterpreter {
  var pos = 0

  override def nextToken(): Option[Token] = ???
}
