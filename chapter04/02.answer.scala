import errorhandling._

println(Some(1).mean(Seq(0.0, 1.0, 2.0, 3.0, 4.0)))
println(None.mean(Seq(0.0, 1.0, 2.0, 3.0, 4.0)))
println(None.mean(Seq()))
