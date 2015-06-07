import errorhandling._

println(Some(1).mean(Seq(0.0, 1.0, 2.0, 3.0, 4.0)))
println(None.mean(Seq(0.0, 1.0, 2.0, 3.0, 4.0)))
println(None.mean(Seq()))
println(Some(1).variance(Seq(0.0, 1.0, 2.0, 3.0, 4.0)))
println(Some(1).variance(Seq(0.0, 2.0, 8.0)))
