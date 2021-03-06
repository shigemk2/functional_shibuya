package datastructures {

  sealed trait List[+A]
  case object Nil extends List[Nothing]
  case class Cons[+A](head: A, tail: List[A]) extends List[A]

  object List {
    def apply[A](as: A*): List[A] = // Variadic function syntax
      if (as.isEmpty) Nil
      else Cons(as.head, apply(as.tail: _*))

    def foldRight[A,B](as: List[A], z: B)(f: (A, B) => B): B =
      as match {
        case Nil => z
        case Cons(x, xs) => f(x, foldRight(xs, z)(f))
      }

    def reverse[A](l: List[A]): List[A] = foldLeft(l, List[A]())((acc,h) => Cons(h,acc))

    @annotation.tailrec
    def foldLeft[A,B](l: List[A], z: B)(f: (B, A) => B): B = l match {
      case Nil => z
      case Cons(h,t) => foldLeft(t, f(z,h))(f)
    }

    def foldRightViaFoldLeft[A,B](l: List[A], z: B)(f: (A,B) => B): B =
      foldLeft(reverse(l), z)((b,a) => f(a,b))

    def foldRightViaFoldLeft_1[A,B](l: List[A], z: B)(f: (A,B) => B): B =
      foldLeft(l, (b:B) => b)((g,a) => b => g(f(a,b)))(z)

    def foldLeftViaFoldRight[A,B](l: List[A], z: B)(f: (B,A) => B): B =
      foldRight(l, (b:B) => b)((a,g) => b => g(f(b,a)))(z)

    def append[A](a1: List[A], a2: List[A]): List[A] =
      a1 match {
        case Nil => a2
        case Cons(h,t) => Cons(h, append(t, a2))
      }

    def concat[A](l: List[List[A]]): List[A] =
      List.foldRight(l, Nil:List[A])(append)
  }

  sealed trait Tree[+A]
  case class Leaf[A](value: A) extends Tree[A]
  case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

  object Tree {
  }
}
