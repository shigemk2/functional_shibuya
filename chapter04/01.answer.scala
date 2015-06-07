import scala.{Option => _, Either => _, _}

sealed trait Option[+A] {
  // answer 01
  def map[B](f: A => B): Option[B] = this match {
    case None => None
    case Some(a) => Some(f(a))
  }

  def getOrElse[B>:A](default: => B): B = this match {
    case None => default
    case Some(a) => a
  }

  def flatMap[B](f: A => Option[B]): Option[B] =
    map(f) getOrElse None

  def flatMap_1[B](f: A => Option[B]): Option[B] = this match {
    case None => None
    case Some(a) => f(a)
  }

  def orElse[B>:A](ob: => Option[B]): Option[B] =
    this map (Some(_)) getOrElse ob

  def orElse_1[B>:A](ob: => Option[B]): Option[B] = this match {
    case None => ob
    case _ => this
  }

  def filter_1(f: A => Boolean): Option[A] =
    flatMap(a => if (f(a)) Some(a) else None)

  def filter(f: A => Boolean): Option[A] = this match {
    case Some(a) if f(a) => Some(a)
    case _ => None
  }
  // answer 02
  def mean(xs: Seq[Double]): Option[Double] =
    if (xs.isEmpty) None
    else Some(xs.sum / xs.length)

  def variance(xs: Seq[Double]): Option[Double] =
    mean(xs) flatMap (m => mean(xs.map(x => math.pow(x - m, 2))))

}
case class Some[+A](get: A) extends Option[A]
case object None extends Option[Nothing]

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

