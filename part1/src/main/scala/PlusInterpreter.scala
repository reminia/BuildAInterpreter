/**
  * Created by slee on 2018/1/27.
  */
trait PlusInterpreter {

  val expr: String

  def pos: Int

  def interpret(): Option[Int] = {
    for {
      left <- nextToken().flatMap(_.get[IntToken])
      plus <- nextToken().flatMap(_.get[PlusToken.type])
      right <- nextToken().flatMap(_.get[IntToken])
    } yield {
      left.value + right.value
    }
  }

  def nextToken(): Option[Token]
}
