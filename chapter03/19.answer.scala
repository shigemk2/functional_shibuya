import datastructures._

def filter[A](l: List[A])(f: A => Boolean): List[A] =
  List.foldRight(l, Nil:List[A])((h,t) => if (f(h)) Cons(h,t) else t)

def filter_1[A](l: List[A])(f: A => Boolean): List[A] =
  List.foldRightViaFoldLeft(l, Nil:List[A])((h,t) => if (f(h)) Cons(h,t) else t)

def filter_2[A](l: List[A])(f: A => Boolean): List[A] = {
  val buf = new collection.mutable.ListBuffer[A]
  def go(l: List[A]): Unit = l match {
    case Nil => ()
    case Cons(h,t) => if (f(h)) buf += h; go(t)
  }
  go(l)
  List(buf.toList: _*)
}

println(List(1,2,3))
println(filter(List(1,2,3))(_ > 1))
println(filter_1(List(1,2,3))(_ > 1))
println(filter_2(List(1,2,3))(_ > 1))
