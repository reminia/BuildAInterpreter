package part8

/**
  * Created by slee on 2018/1/27.
  */
sealed trait Token

case class IntToken(value: Int) extends Token

case object EOFToken extends Token

abstract class Operator extends Token {
  def apply(a: Int, b: Int): Int
}

object Plus extends Operator {
  override def apply(a: Int, b: Int): Int = a + b

  override def toString = "+"
}

object Minus extends Operator {
  override def apply(a: Int, b: Int): Int = a - b

  override def toString = "-"
}

object Multiplication extends Operator {
  override def apply(a: Int, b: Int): Int = a * b

  override def toString = "*"
}

object Divide extends Operator {
  override def apply(a: Int, b: Int): Int = a / b //loose integral part

  override def toString = "/"
}

case object LeftPara extends Token

case object RightPara extends Token
