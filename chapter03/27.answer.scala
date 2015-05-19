import datastructures._

def depth[A](t: Tree[A]): Int = t match {
  case Leaf(_) => 0
  case Branch(l,r) => 1 + (depth(l) max depth(r))
}

println(Leaf(1))
println(depth(Leaf(1)))
println(depth(Branch(Leaf(3),Leaf(1))))
println(depth(Branch(Leaf(1),Branch(Leaf(2),Leaf(3)))))
println(depth(Branch(Branch(Leaf(1),Leaf(2)), Branch(Leaf(3),Leaf(4)))))
