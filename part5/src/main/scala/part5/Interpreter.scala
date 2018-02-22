package part5

/**
  * Created by slee on 2018/1/27.
  */
trait Interpreter {

  val expr: String

  def pos: Int

  def interpret(): Int

  def nextToken(): Option[Token]
}
