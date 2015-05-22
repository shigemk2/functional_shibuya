package errorhandling {
  import scala.{Option => _, Either => _, _}

  sealed trait Option[+A] {
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
  }
  case class Some[+A](get: A) extends Option[A]
  case object None extends Option[Nothing]
}
