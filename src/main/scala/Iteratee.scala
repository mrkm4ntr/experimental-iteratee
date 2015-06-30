package experminental

import scalaz._
import Scalaz._

sealed trait IterVM[E, F[_], A] {
  //def run: F[Option[A]]
}

case class Iteratee[E, F[_]: Monad, A](runIter: F[IterVM[E, F, A]]) {
  /*def $$(enum: Enumrator[E, F, A]): Iteratee[E, F, A] =
    Iteratee { implicitly[Monad[F]].bind(this.runIter)(enum) }*/
}

case class DoneM[E, F[_], A](a: A, str: StreamG[E]) extends IterVM[E, F, A] {
  //def run = implicitly[Monad[F]].point(a.some)
}

case class ContM[E, F[_], A](step: StreamG[E] => Iteratee[E, F, A]) extends IterVM[E, F, A] {
  /*def run = implicitly[Monad[F]].point[Option[A]](step(EOF).runIter match {
    case DoneM(a: A, _) => Some(a)
    case _ => None
  })*/
}

object Instances {

  implicit def IterateeMonad[E, F[_]: Monad] = new Monad[({type X[A] = Iteratee[E, F, A]})#X] {
    def point[A](a: => A) = Iteratee { implicitly[Monad[F]].point(DoneM[E, F, A](a, Empty)) }
    def bind[A, B](fa: Iteratee[E, F, A])(f: A => Iteratee[E, F, B]): Iteratee[E, F, B] = Iteratee {
      implicitly[Monad[F]].bind(fa.runIter)({
        case DoneM(a, str) => f(a).runIter
        case ContM(s) => implicitly[Monad[F]].point(ContM {
          (str: StreamG[E]) => bind(s(str))(f)
        })
      })
    }
  }

  implicit def IterateeMonadTrans[E] = new MonadTrans[({type X[F[_], A] = Iteratee[E, F, A]})#X] {
    def liftM[G[_]: Monad, A](a: G[A]): Iteratee[E, G, A] = Iteratee {
      a >>= (x => implicitly[Monad[G]].point(DoneM[E, G, A](x, Empty)))
    }
    implicit def apply[G[_]: Monad] = IterateeMonad[E, G]
    /*implicit def apply[G[_]: Monad] = new Monad[({type Y[A] = Iteratee[E, G, A]})#Y] {
      def point[A](a: => A) = Iteratee{ implicitly[Monad[G]].point(DoneM[E, G, A](a, Empty)) }
      def bind[A, B](fa: Iteratee[E, G, A])(f: A => Iteratee[E, G, B]): Iteratee[E, G, B] = Iteratee {
        implicitly[Monad[G]].bind(fa.runIter)({
          case DoneM(a, str) => f(a).runIter
          case ContM(s) => implicitly[Monad[G]].point(ContM {
            (str: StreamG[E]) => bind(s(str))(f)
          })
        })
      }
    }*/
  }
}

object EB {
  def head[F[_]: Monad]: Iteratee[Char, F, Option[Int]] = {
    def step(el: StreamG[Char]): Iteratee[Char, F, Option[Int]] = Iteratee {
      implicitly[Monad[F]].point(
        el match {
          case El(el) => DoneM[Char, F, Option[Int]](Some(el.toInt), Empty)
          case Empty => ContM[Char, F, Option[Int]](step)
          case EOF => DoneM[Char, F, Option[Int]](None, EOF)
        }
      )
    }
    Iteratee { implicitly[Monad[F]].point(ContM(step)) }
  }
}
