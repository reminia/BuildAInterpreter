import scala.language.implicitConversions

/**
  * Created by slee on 2018/1/28.
  */
package object part4 {

  implicit def toOption[T](v: T): Option[T] = Some(v)

  implicit def intToToken(a: Int): IntToken = IntToken(a)
}
