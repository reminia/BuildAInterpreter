import scala.language.implicitConversions
import scala.util.{Failure, Success, Try}

/**
  * Created by slee on 2018/1/28.
  */
package object part5 {

  implicit def toOption[T](v: T): Option[T] = Some(v)

  implicit def intToToken(a: Int): IntToken = IntToken(a)

  implicit def tokenToInt(a: IntToken): Int = a.value

  def logger(exp: => Any): Unit = Try(exp) match {
    case Success(v) => println(v)
    case Failure(e) => println(e)
  }
}
