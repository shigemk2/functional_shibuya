import datastructures._

def size[A](t: Tree[A]): Int = t match {
  case Leaf(_) => 1
  case Branch(l,r) => 1 + size(l) + size(r)
}

println(Leaf(1))
println(size(Leaf(1)))
println(size(Branch(Leaf(1),Leaf(1))))
println(size(Branch(Leaf(1),Branch(Leaf(1),Leaf(1)))))
println(size(Branch(Branch(Leaf(1),Leaf(1)), Branch(Leaf(1),Leaf(1)))))
