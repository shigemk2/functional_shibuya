import errorhandling._

def map2[A,B,C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] =
  a flatMap (aa => b map (bb => f(aa, bb)))

println(map2(Some(5), Some(2))((a: Int, b: Int) => a + b))
