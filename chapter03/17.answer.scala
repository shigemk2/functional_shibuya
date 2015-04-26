import datastructures._

def doubleToString(l: List[Double]): List[String] =
  List.foldRight(l, Nil:List[String])((h,t) => Cons(h.toString,t))

println(doubleToString(List(1.0, 2.0, 3.0)))
