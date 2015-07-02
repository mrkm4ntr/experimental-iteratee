package experminental

import scalaz._
import Scalaz._

case class Enumerator[E, F[_]: Monad, A](unwrap: IterVM[E, F, A] => F[IterVM[E, F, A]]) { self =>
  def >=>(e: Enumerator[E, F, A]) = Enumerator { (Kleisli{ self.unwrap } >=> Kleisli { e.unwrap }).run }
}

object Enumerator {

  import effect._
  import IO._

  def enumList[E, F[_]: Monad, A](l: List[E]): Enumerator[E, F, A] = {
    def loop(l: List[E])(iter: IterVM[E, F, A]): F[IterVM[E, F, A]] = iter match {
      case DoneM(_, _) => implicitly[Monad[F]].point(iter)
      case ContM(s) => l match {
        case Nil => implicitly[Monad[F]].point(iter)
        case x :: xs => s(El(x)).runIter.flatMap(loop(xs))
      }
    }
    Enumerator(loop(l))
  }

  def enumHandle[A](bf: java.io.BufferedReader): Enumerator[Char, IO, A] = {
    def loop(iter: IterVM[Char, IO, A]): IO[IterVM[Char, IO, A]] = iter match {
      case DoneM(_, _) => iter.point[IO]
      case ContM(s) => {
        val i = bf.read()
        if (i == -1) iter.point[IO]
        else s(El(i.toChar)).runIter
      }
    }
    Enumerator(loop)
  }

  implicit val bfrResource = Resource.resourceFromCloseable[java.io.BufferedReader]

  def enumFile[A](path: String): Enumerator[Char, IO, A] = Enumerator { iter =>
    IO(new java.io.BufferedReader(new java.io.FileReader(path))).using { bf =>
      enumHandle[A](bf).unwrap(iter)
    }
  }
}
