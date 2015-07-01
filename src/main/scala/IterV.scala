package experminental

import scalaz._
import Scalaz._

sealed trait IterV[E, A] {
  def enum(input: List[E]): IterV[E, A]
  def run: Option[A]
  def liftIter[F[_]: Monad]: Iteratee[E, F, A]
}

case class Done[E, A](a: A, last: StreamG[E]) extends IterV[E, A] {
  def enum(input: List[E]) = this
  def run = Option(a)
  def liftIter[F[_]: Monad] = Iteratee { implicitly[Monad[F]].point(DoneM[E, F, A](a, last)) }
}

case class Cont[E, A](step: StreamG[E] => IterV[E, A]) extends IterV[E, A] {
  def enum(input: List[E]) = input match {
    case x :: xs => step(El(x)).enum(xs)
    case _ => this
  }
  def run = step(EOF) match {
    case Done(a, _) => Some(a)
    case _ => None
  }
  def liftIter[F[_]: Monad] = 
    Iteratee(implicitly[Monad[F]].point(ContM[E, F, A]{ str => step(str).liftIter[F] }))
}

object IterV {
  def head[E]: IterV[E, Option[E]] = {
    def step: StreamG[E] => IterV[E, Option[E]] = {
      case El(el) => Done(Some(el), Empty)
      case Empty => Cont(step)
      case EOF => Done(None, EOF)
    }
    Cont(step)
  }

  def length[E]: IterV[E, Int] = {
    def step[E](acc: Int): StreamG[E] => IterV[E, Int] = {
      case El(el) => Cont(step(acc + 1))
      case Empty => Cont(step(acc))
      case EOF => Done(acc, EOF)
    }
    Cont(step(0))
  }

  implicit def IterVMonad[E] = new Monad[({type X[A] = IterV[E, A]})#X] {
    def point[A](a: => A) = Done(a, Empty)
    def bind[A, B](fa: IterV[E, A])(f: A => IterV[E, B]): IterV[E, B] = fa match {
      case Done(a, str) => f(a) match {
        case Done(b, _) => Done(b, str)
        case Cont(s) => s(str)
      }
      case Cont(s) => Cont { str => bind(s(str))(f) }
    }
  }
}
