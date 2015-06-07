import errorhandling._

println(Some(1))
println(Some(1).map((x: Int) => x + 1))
println(Some(4).map2(Some(5), Some(2))((a: Int, b: Int) => a + b))
