package it.polentino.redbook

import org.scalatest.Checkpoints
import org.scalatest.flatspec.AnyFlatSpec

class Chapter04_1Spec extends AnyFlatSpec with Checkpoints {

  sealed trait Either[+E, +A] {
    def map[B](f: A => B): Either[E, B] = this match {
      case l: Left[E]   => l
      case Right(value) => Right(f(value))
    }

    def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] =
      this match {
        case l: Left[E]   => l
        case Right(value) => f(value)
      }

    // not much of a sense to implement it since we now want to accumulate errors...
    //      def orElse[EE >: E, B >: A](b: Either[EE, B]): Either[EE, B] =
    //        this match {
    //          case Left(_)     => b
    //          case r: Right[A] => r
    //        }

    def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] =
      this match {
        case Left(e1) =>
          b match {
            case Left(e2) => Left(e1 ++ e2)
            case _        => Left(e1)
          }
        case Right(aa) =>
          b match {
            case Left(e2)  => Left(e2)
            case Right(bb) => Right(f(aa, bb))
          }
      }
  }

  case class Left[+E](values: List[E]) extends Either[E, Nothing]

  case class Right[+A](value: A) extends Either[Nothing, A]
  "[EX 4.8] change Either definition to accumulate errors" should "work" in {

    // note: same impl as in Chapter04_0Spec
    def traverse[E, A, B](as: List[A])(
        f: A => Either[E, B]
    ): Either[E, List[B]] = as match {
      case Nil            => Right(Nil)
      case ::(head, tail) => (f(head) map2 traverse(tail)(f))(_ :: _)
    }

    // note: same impl as in Chapter04_0Spec
    def sequence[E, A](es: List[Either[E, A]]): Either[E, List[A]] =
      traverse(es)(identity)

    val le = List(Right(1), Right(2), Right(3))
    val ls = List(1, 2, 3)

    val c = new Checkpoint
    assert(traverse(ls)(v => Right(v + 1)) == Right(List(2, 3, 4)))
    assert(
      traverse(ls)(i => Left(List(s"ouch_${i}"))) == Left(
        List("ouch_1", "ouch_2", "ouch_3")
      )
    )
    assert(
      traverse(ls)(i =>
        if (i == 2) Left(List(s"ouch_${i}")) else Right(i)
      ) == Left(
        List("ouch_2")
      )
    )

    assert(sequence(le) == Right(ls))
    assert(sequence(le :+ Left(List("ouch"))) == Left(List("ouch")))
    c.reportAll()
  }
}
