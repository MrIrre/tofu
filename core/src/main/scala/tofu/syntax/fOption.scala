package tofu.syntax

import cats.Monad
import cats.instances.option._
import cats.syntax.traverse._
import tofu.Raise

object fOption {
  implicit final class TofuFOptionOps[F[_]: Monad, A](private val fOptA: F[Option[A]]) {
    def getOrElseF(fa: => F[A]): F[A] =
      Monad[F].flatMap(fOptA)(_.fold(fa)(Monad[F].pure))

    def orElseF(fa: => F[Option[A]]): F[Option[A]] =
      Monad[F].flatMap(fOptA) {
        case s @ Some(_) => Monad[F].pure(s)
        case None        => fa
      }

    def orThrow[E](err: => E)(implicit FE: Raise[F, E]): F[A] =
      getOrElseF(FE.raise(err))

    def flatMapOpt[B](f: A => F[B]): F[Option[B]] =
      Monad[F].flatMap(fOptA)(_.traverse(f))

    def doubleFlatMap[B](f: A => F[Option[B]]): F[Option[B]] =
      Monad[F].flatMap(fOptA)(_.flatTraverse(f))
  }
}
