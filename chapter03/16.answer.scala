import datastructures._

def add1(l: List[Int]): List[Int] =
  List.foldRight(l, Nil:List[Int])((h,t) => Cons(h+1,t))


val a = Cons(1,Cons(2,Cons(3,Nil)))
val b = List(1,2,3,4)
println(a)
println(b)
println(add1(a))
println(add1(List(3,4,5)))
