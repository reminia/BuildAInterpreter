import Implicits._
/**
  * Created by slee on 2018/1/27.
  */
sealed trait Token {
  def get[T]: Option[T] = {
    this match {
      case (a: T) => a.asInstanceOf[T]
      case _ => None
    }
  }
}

case class IntToken(value: Int) extends Token

case object PlusToken extends Token

case object EOFToken extends Token

object Test extends App {
  val t: Token =  IntToken(3)
  t match {
    case IntToken(a) => println(a)
    case EOFToken =>  println("eof")
    case _ => throw new Exception("unrecognized character")
  }
  println(t.isInstanceOf[IntToken])
  val value = t match {
    case a: IntToken => a.asInstanceOf[IntToken].value
  }
  println(value)
}

