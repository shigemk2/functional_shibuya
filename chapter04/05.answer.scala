import errorhandling._

def map2[A,B,C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] =
  a flatMap (aa => b map (bb => f(aa, bb)))

def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] =
  a match {
    case Nil => Some(Nil)
    case h::t => map2(f(h), traverse(t)(f))(_ :: _)
  }

println(traverse(List(1,2,3))((a: Int) => Some(a + 5)))
