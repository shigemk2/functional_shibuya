import datastructures._

// foldRightを使う方法
def map[A,B](l: List[A])(f: A => B): List[B] =
  List.foldRight(l, Nil:List[B])((h,t) => Cons(f(h),t))

// foldRightViaFoldLeftを使う方法
def map_1[A,B](l: List[A])(f: A => B): List[B] =
  List.foldRightViaFoldLeft(l, Nil:List[B])((h,t) => Cons(f(h),t))

def map_2[A,B](l: List[A])(f: A => B): List[B] = {
  val buf = new collection.mutable.ListBuffer[B]
  def go(l: List[A]): Unit = l match {
    case Nil => ()
    case Cons(h,t) => buf += f(h); go(t)
  }
  go(l)
  List(buf.toList: _*) // converting from the standard Scala list to the list we've defined here
}

println(List(1,2,3))
println(map(List(1,2,3))((x: Int) => x + 1))
println(map_1(List(1,2,3))((x: Int) => x + 1))
println(map_2(List(1,2,3))((x: Int) => x + 1))
