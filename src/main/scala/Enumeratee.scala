package experminental

case class Enumeratee[E1, E2, A](unwrap: IterV[E1, A] => IterV[E2, IterV[E1, A]])

object Enumeratee {

  def filter[E, A](p: E => Boolean): Enumeratee[E, E, A] = {
    def step(k: StreamG[E] => IterV[E, A]): StreamG[E] => IterV[E, IterV[E, A]] = {
      case e@El(el) if (p(el)) => filter(p).unwrap(k(e))
      case EOF => Done(k(EOF), EOF)
      case _ => Cont(step(k))
    }
    
    Enumeratee {
      case Cont(k) => Cont(step(k))
      case i@Done(a, str) => Done(i, str)
    }
  }

}
