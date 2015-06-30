package experminental

sealed trait StreamG[+A]

case object Empty extends StreamG[Nothing]
case class El[A](el: A) extends StreamG[A]
case object EOF extends StreamG[Nothing]
