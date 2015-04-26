import datastructures._

@annotation.tailrec
def startsWith[A](l: List[A], prefix: List[A]): Boolean = (l,prefix) match {
  case (_,Nil) => true
  case (Cons(h,t),Cons(h2,t2)) if h == h2 => startsWith(t, t2)
  case _ => false
}
@annotation.tailrec
def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean = sup match {
  case Nil => sub == Nil
  case _ if startsWith(sup, sub) => true
  case Cons(_,t) => hasSubsequence(t, sub)
}

println(List(1,2,3))
println(startsWith(List(1,2,3), List(1)))
println(startsWith(List(1,2,3), List(2)))

println(hasSubsequence(List(1,2,3), List(1)))
println(hasSubsequence(List(1,2,3), List(2)))
println(hasSubsequence(List(1,2,3), List(2,3)))
println(hasSubsequence(List(1,2,3), List(1,2,3)))
println(hasSubsequence(List(1,2,3), List(3,4)))
