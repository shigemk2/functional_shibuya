import datastructures._

def foldRight[A,B](as: List[A], z: B)(f: (A, B) => B): B =
  as match {
    case Nil => z
    case Cons(x, xs) => f(x, foldRight(xs, z)(f))
  }

def add1(l: List[Int]): List[Int] =
  foldRight(l, Nil:List[Int])((h,t) => Cons(h+1,t))


val a = Cons(1,Cons(2,Cons(3,Nil)))
val b = List(1,2,3,4)
println(a)
println(b)
println(add1(a))
println(add1(List(3,4,5)))
