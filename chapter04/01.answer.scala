import errorhandling._

println(Some(1))
println(Some(1).map((x: Int) => x + 1))

println(Some(1).getOrElse(1))
val v1 = List(Some("A"), None)
for(each <- v1) println(each.getOrElse("empty"))

println(Some(1).flatMap((x: Int) => Some(x + 1)))
println(None.flatMap((x: Int) => Some(x + 1)))

println(Some(1).flatMap_1((x: Int) => Some(x + 1)))
println(None.flatMap_1((x: Int) => Some(x + 1)))

println(Some(1).orElse(Some(1)))
val v2 = List(Some("A"), None)
for(each <- v2) println(each.orElse(Some("empty")))

println(Some(1).orElse_1(Some(1)))
val v3 = List(Some("A"), None)
for(each <- v3) println(each.orElse_1(Some("empty")))

println(Some(1).filter((x: Int) => x > 0))
println(Some(0).filter((x: Int) => x > 0))
println(None.filter((x: Int) => x > 0))

println(Some(1).filter_1((x: Int) => x > 0))
println(Some(0).filter_1((x: Int) => x > 0))
println(None.filter_1((x: Int) => x > 0))

