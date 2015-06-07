import errorhandling._
import errorhandling.Option._

println(map2(Some(5), Some(2))((a: Int, b: Int) => a + b))
