/**
  * Created by slee on 2018/1/27.
  */
object Implicits {

  implicit def toOption[T](v: T): Option[T] = Some(v)
}
