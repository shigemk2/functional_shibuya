case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty
    else cons(as.head, apply(as.tail: _*))

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] =
    f(z) match {
      case Some((h,s)) => cons(h, unfold(s)(f))
      case None => empty
    }

  // 12
  val fibsViaUnfold =
    unfold((0,1)) { case (f0,f1) => Some((f0,(f1,f0+f1))) }

  def fromViaUnfold(n: Int) =
    unfold(n)(n => Some((n,n+1)))

  def constantViaUnfold[A](a: A) =
    unfold(a)(_ => Some((a,a)))

  val onesViaUnfold = unfold(1)(_ => Some((1,1)))
}

import Stream._
trait Stream[+A] {
  def toList: List[A] = {
    @annotation.tailrec
    def go(s: Stream[A], acc: List[A]): List[A] = s match {
      case Cons(h,t) => go(t(), h() :: acc)
      case _ => acc
    }
    go(this, List()).reverse
  }

  def take(n: Int): Stream[A] = this match {
    case Cons(h, t) if n > 1 => cons(h(), t().take(n - 1))
    case Cons(h, _) if n == 1 => cons(h(), empty)
    case _ => empty
  }
}

object Answer12 {
  import Stream._
  def main(args: Array[String]) = {
    println(Stream.fibsViaUnfold.take(10).toList) // List(0, 2, 6, 14, 30, 62, 126, 254, 510, 1022)
    println(Stream.fromViaUnfold(10).take(10).toList)
    println(Stream.constantViaUnfold(10).take(10).toList)
    println(Stream.onesViaUnfold.take(10).toList)
    // List(0, 1, 1, 2, 3, 5, 8, 13, 21, 34)
    // List(10, 11, 12, 13, 14, 15, 16, 17, 18, 19)
    // List(10, 10, 10, 10, 10, 10, 10, 10, 10, 10)
    // List(1, 1, 1, 1, 1, 1, 1, 1, 1, 1)
  }
}
