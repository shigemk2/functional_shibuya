import scala.{Option => _, Either => _, _}

sealed trait Either[+E,+A] {
  def map[B](f: A => B): Either[E, B] =
    this match {
      case Right(a) => Right(f(a))
      case Left(e) => Left(e)
    }

  def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] =
    this match {
      case Left(e) => Left(e)
      case Right(a) => f(a)
    }
  def orElse[EE >: E, AA >: A](b: => Either[EE, AA]): Either[EE, AA] =
    this match {
      case Left(_) => b
      case Right(a) => Right(a)
    }
  def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C):
      Either[EE, C] = for { a <- this; b1 <- b } yield f(a,b1)
}
case class Left[+E](get: E) extends Either[E,Nothing]
case class Right[+A](get: A) extends Either[Nothing,A]

println(Right(1))
println(Left("left"))

println(Right(1).map(_.toFloat))
println(Right(1).flatMap((x: Int) => Right(x.toFloat)))
val v1 = List(Right("A"), Left("B"))
for(each <- v1) println(each.orElse(Left("empty")))
println(Right(1).map2(Right(5))((a: Int, b: Int) => a + b))
