import datastructures._

def map[A,B](t: Tree[A])(f: A => B): Tree[B] = t match {
  case Leaf(a) => Leaf(f(a))
  case Branch(l,r) => Branch(map(l)(f), map(r)(f))
}

println(Leaf(1))
println(map(Leaf(1))((x: Int) => x * 2))
println(map(Branch(Leaf(3),Leaf(1)))((x: Int) => x * 2))
println(map(Branch(Leaf(1),Branch(Leaf(2),Leaf(3))))((x: Int) => x * 2))
println(map(Branch(Branch(Leaf(1),Leaf(2)), Branch(Leaf(3),Leaf(4))))((x: Int) => x * 2))
