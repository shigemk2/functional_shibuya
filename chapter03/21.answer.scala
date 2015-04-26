import datastructures._

def map[A,B](l: List[A])(f: A => B): List[B] =
  List.foldRight(l, Nil:List[B])((h,t) => Cons(f(h),t))

def flatMap[A,B](l: List[A])(f: A => List[B]): List[B] =
  List.concat(map(l)(f))

def filterViaFlatMap[A](l: List[A])(f: A => Boolean): List[A] =
  flatMap(l)(a => if (f(a)) List(a) else Nil)

println(List(1,2,3))
println(filterViaFlatMap(List(1,2,3))(_ > 1))
