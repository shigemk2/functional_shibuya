val p = ("Bob", 42)

println(p._1)

println(p._2)

println(p match { case (a,b) => b })

// アリティ(タプルのオペランドの個数)が大きいタプルも動作する
val q = ("Bob", 1,2,3,4,5,6)

println(q._6)

println(q match { case (a,b,c,d,e,f,g) => b })
