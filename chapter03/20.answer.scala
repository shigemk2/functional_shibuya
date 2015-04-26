import datastructures._

def map[A,B](l: List[A])(f: A => B): List[B] =
  List.foldRight(l, Nil:List[B])((h,t) => Cons(f(h),t))

def flatMap[A,B](l: List[A])(f: A => List[B]): List[B] = 
  List.concat(map(l)(f))

println(List(1,2,3))
println(flatMap(List(1,2,3))(i => List(i, i)))
