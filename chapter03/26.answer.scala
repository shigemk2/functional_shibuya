import datastructures._

def maximum(t: Tree[Int]): Int = t match {
  case Leaf(n) => n
  case Branch(l,r) => maximum(l) max maximum(r)
}

println(Leaf(1))
println(maximum(Leaf(1)))
println(maximum(Branch(Leaf(3),Leaf(1))))
println(maximum(Branch(Leaf(1),Branch(Leaf(2),Leaf(3)))))
println(maximum(Branch(Branch(Leaf(1),Leaf(2)), Branch(Leaf(3),Leaf(4)))))
