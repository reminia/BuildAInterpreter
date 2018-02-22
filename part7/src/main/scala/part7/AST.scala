package part7

/**
  * Created by slee on 2018/2/22.
  */
trait AST

case class Number(value: Int) extends AST {
  override def toString = value.toString
}

case class BinOp(left: AST, right: AST, op: Operator) extends AST {

  override def toString: String = s"$op($left, $right)"
}
