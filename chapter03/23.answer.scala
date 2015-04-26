import datastructures._

def addPairwise(a: List[Int], b: List[Int]): List[Int] = (a,b) match {
  case (Nil, _) => Nil
  case (_, Nil) => Nil
  case (Cons(h1,t1), Cons(h2,t2)) => Cons(h1+h2, addPairwise(t1,t2))
}

def zipWith[A,B,C](a: List[A], b: List[B])(f: (A,B) => C): List[C] = (a,b) match {
  case (Nil, _) => Nil
  case (_, Nil) => Nil
  case (Cons(h1,t1), Cons(h2,t2)) => Cons(f(h1,h2), zipWith(t1,t2)(f))
}

println(List(1,2,3))
println(addPairwise(List(1,2,3), List(4,5,6)))
println(zipWith(List(1,2,3), List(4,5,6))((x: Int, y:Int) => x - y))
