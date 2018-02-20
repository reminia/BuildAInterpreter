package part3

/**
  * Created by slee on 2018/1/27.
  */
trait Interpreter {

  val expr: String

  def pos: Int

  def interpret(): Option[Int] = {
    for {
      left <- nextToken().flatMap(_.get[IntToken])
      ops <- nextToken().flatMap(_.get[Operator])
      right <- nextToken().flatMap(_.get[IntToken])
    } yield {
      ops(left.value, right.value)
    }
  }

  def nextToken(): Option[Token]
}
