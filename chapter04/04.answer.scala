import errorhandling._
import errorhandling.Option._

println(Some(1))
println(Some(1).map((x: Int) => x + 1))
println(sequence[Int](List(Some(5), Some(2))))
