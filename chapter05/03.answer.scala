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

  val ones: Stream[Int] = Stream.cons(1, ones)

}

import Stream._
trait Stream[+A] {
  // 01
  def toList: List[A] = {
    @annotation.tailrec
    def go(s: Stream[A], acc: List[A]): List[A] = s match {
      case Cons(h,t) => go(t(), h() :: acc)
      case _ => acc
    }
    go(this, List()).reverse
  }

  // 03
  def takeWhile(f: A => Boolean): Stream[A] = this match {
    case Cons(h,t) if f(h()) => cons(h(), t() takeWhile f)
    case _ => empty
  }
}

object Answer03 {
  import Stream._
  def main(args: Array[String]) = {
    println(Stream(1,2,3,4,5).takeWhile(n => n > 1).toList) // からのリスト
  }
}
