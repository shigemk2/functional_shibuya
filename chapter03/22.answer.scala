import datastructures._

def addPairwise(a: List[Int], b: List[Int]): List[Int] = (a,b) match {
  case (Nil, _) => Nil
  case (_, Nil) => Nil
  case (Cons(h1,t1), Cons(h2,t2)) => Cons(h1+h2, addPairwise(t1,t2))
}

println(List(1,2,3))
println(addPairwise(List(1,2,3), List(4,5,6)))
println(addPairwise(List(1,2,3), List(4,5,6,7)))
