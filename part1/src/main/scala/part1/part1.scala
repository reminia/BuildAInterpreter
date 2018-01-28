/**
  * Created by slee on 2018/1/28.
  */
package object part1 {

  implicit def toOption[T](v: T): Option[T] = Some(v)
}
