import datastructures._

def fold[A,B](t: Tree[A])(f: A => B)(g: (B,B) => B): B = t match {
  case Leaf(a) => f(a)
  case Branch(l,r) => g(fold(l)(f)(g), fold(r)(f)(g))
}

def sizeViaFold[A](t: Tree[A]): Int =
  fold(t)(a => 1)(1 + _ + _)

def maximumViaFold(t: Tree[Int]): Int =
  fold(t)(a => a)(_ max _)

def depthViaFold[A](t: Tree[A]): Int =
  fold(t)(a => 0)((d1,d2) => 1 + (d1 max d2))

println(Leaf(1))
println(maximumViaFold(Branch(Leaf(3),Leaf(1))))
println(depthViaFold(Branch(Leaf(3),Leaf(1))))
println(sizeViaFold(Branch(Leaf(3),Leaf(1))))
println(fold(Leaf(1))((x: Int) => x * 2)((y: Int, z: Int) => y + z))
println(fold(Branch(Leaf(3),Leaf(1)))((x: Int) => x * 2)((y: Int, z: Int) => y + z))
