import Implicits._

import scala.reflect.ClassTag
/**
  * Created by slee on 2018/1/27.
  */
sealed trait Token {
  def get[T: ClassTag]: Option[T] = {
    this match {
      case t: T => t
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

  //type erase of T
  def f[T](t: Token): Option[T] = {
    t match {
      case _:T  => t.asInstanceOf[T] //always true
      case _ => None
    }
  }

  val e: Token = EOFToken
  println(e.get[IntToken])
  println (f[IntToken](e))
}

