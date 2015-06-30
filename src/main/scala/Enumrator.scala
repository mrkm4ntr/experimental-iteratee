package experminental

case class Enumrator[E, F[_], A](run: IterVM[E, F, A] => F[IterVM[E, F, A]])
