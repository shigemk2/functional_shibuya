import errorhandling._

println(Some(1))
println(Some(1).map((x: Int) => x + 1))

println(Some(1).getOrElse(1))
val v = List(Some("A"), None)
for(each <- v) println(each.getOrElse("empty"))

println(Some(1).flatMap((x: Int) => Some(x + 1)))
println(None.flatMap((x: Int) => Some(x + 1)))

println(Some(1).flatMap_1((x: Int) => Some(x + 1)))
println(None.flatMap_1((x: Int) => Some(x + 1)))

println(Some(1).orElse(Some(1)))
val w = List(Some("A"), None)
for(each <- w) println(each.orElse(Some("empty")))

