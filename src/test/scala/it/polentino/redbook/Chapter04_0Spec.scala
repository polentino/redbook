package it.polentino.redbook

import org.scalatest.Checkpoints
import org.scalatest.flatspec.AnyFlatSpec

import scala.util.Try

class Chapter04_0Spec extends AnyFlatSpec with Checkpoints {
  sealed trait Option[+A] {
    def map[B](f: A => B): Option[B] = this match {
      case Some(get) => Some(f(get))
      case None      => None
    }

    def flatMap[B](f: A => Option[B]): Option[B] = map(f).getOrElse(None)

    def getOrElse[B >: A](default: => B): B = this match {
      case Some(get) => get
      case None      => default
    }

    def orElse[B >: A](default: => Option[B]): Option[B] =
      map(Some(_)).getOrElse(default)

    def filter(f: A => Boolean): Option[A] = this match {
      case s @ Some(get) if f(get) => s
      case _                       => None
    }
  }
  case class Some[+A](get: A) extends Option[A]
  case object None extends Option[Nothing]

  "[EX 4.1] implementation of all Option methods" should "work" in {
    val c = new Checkpoint
    assert(Some(2).map(_ + 1) == Some(3))
    assert((None: Option[Int]).map(_ + 1) == None)

    assert(Some(2).flatMap(v => Some(v + 1)) == Some(3))
    assert(Some(2).flatMap(v => None) == None)
    assert((None: Option[Int]).flatMap(v => Some(v + 1)) == None)
    assert((None: Option[Int]).flatMap(v => None) == None)

    assert(Some(2).getOrElse(5) == 2)
    assert((None: Option[Int]).getOrElse(5) == 5)

    assert(Some(2).orElse(Some(5)) == Some(2))
    assert((None: Option[Int]).orElse(Some(5)) == Some(5))

    assert(Some(2).filter(_ == 2) == Some(2))
    assert(Some(2).filter(_ == 3) == None)
    assert((None: Option[Int]).filter(_ == 2) == None)
    assert((None: Option[Int]).filter(_ == 3) == None)
    c.reportAll()
  }

  "[EX 4.2] implementation of variance method" should "work" in {
    def mean(xs: Seq[Double]): Option[Double] =
      if (xs.isEmpty) None else Some(xs.sum / xs.length)

    def variance(xs: Seq[Double]): Option[Double] =
      mean(xs).flatMap(m => mean(xs.map(x => math.pow(x - m, 2))))

    val c = new Checkpoint
    assert(variance(Seq.empty) == None)
    assert(variance(Seq(3, 5, 8, 1)) == Some(6.6875))
    c.reportAll()
  }

  "[EX 4.3] implementation of map2 method" should "work" in {
    def map2[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] =
      a.flatMap(aa => b.map(bb => f(aa, bb)))

    val c = new Checkpoint
    assert(map2(Some(1), Some(2))(_ + _) == Some(3))
    assert(map2((None: Option[Int]), Some(2))(_ + _) == None)
    assert(map2(Some(1), None)(_ + _) == None)
    assert(map2((None: Option[Int]), (None: Option[Int]))(_ + _) == None)
    c.reportAll()
  }

  "[EX 4.4] implementation of sequence method" should "work" in {
    def sequence[A](a: List[Option[A]]): Option[List[A]] = a match {
      case Some(head) :: Nil  => Some(List(head))
      case Some(head) :: tail => sequence(tail).map(head +: _)
      case _                  => None
    }

    val c = new Checkpoint
    assert(sequence(List(Some(1))) == Some(List(1)))
    assert(sequence(List(None)) == None)
    assert(sequence(List(Some(1), Some(2), Some(3))) == Some(List(1, 2, 3)))
    assert(sequence(List(Some(1), None, Some(3))) == None)
    c.reportAll()
  }

  "[EX 4.5] implementation of traverse method" should "work" in {
    def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] =
      a match {
        case Nil           => None
        case ::(head, Nil) => f(head).map(List(_))
        case ::(head, next) =>
          f(head).flatMap(h => traverse(next)(f).map(h +: _))
      }

    val l = List("1", "2", "3")
    val f = (s: String) => if (s.isBlank) None else Some(s)

    val c = new Checkpoint
    assert(traverse(Nil)(f) == None)
    assert(traverse(l)(f) == Some(l))
    assert(traverse(List(" ", "2", "3"))(f) == None)
    assert(traverse(List("1", "2", " "))(f) == None)
    c.reportAll()
  }

  sealed trait Either[+E, +A] {
    def map[B](f: A => B): Either[E, B] = this match {
      case l: Left[E]   => l
      case Right(value) => Right(f(value))
    }

    def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] = this match {
      case l: Left[E]   => l
      case Right(value) => f(value)
    }

    def orElse[EE >: E, B >: A](b: Either[EE, B]): Either[EE, B] = this match {
      case Left(_)     => b
      case r: Right[A] => r
    }

    def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] =
      for {
        aa <- this
        bb <- b
      } yield f(aa, bb)
  }
  case class Left[+E](value: E) extends Either[E, Nothing]
  case class Right[+A](value: A) extends Either[Nothing, A]

  "[EX 4.6] implementation of all Either methods" should "work" in {
    val c = new Checkpoint
    assert(Right(2).map(_ + 2) == Right(4))
    assert((Left("err"): Either[String, Int]).map(_ + 2) == Left("err"))

    assert(Right(2).flatMap(v => Right(v + 2)) == Right(4))
    assert(Right(2).flatMap(v => Left("e")) == Left("e"))
    assert(
      (Left("err"): Either[String, Int]).flatMap(v => Right(v + 2)) == Left(
        "err"
      )
    )

    assert(Right(2).orElse(Right(5)) == Right(2))
    assert((Left("err"): Either[String, Int]).orElse(Right(5)) == Right(5))

    assert(Right(2).map2(Right(3))(_ + _) == Right(5))
    assert(Right(2).map2(Left("bb"))(_ + _) == Left("bb"))
    assert(
      (Left("aa"): Either[String, Int]).map2(Right(3))(_ + _) == Left("aa")
    )
    c.reportAll()
  }

  "[EX 4.7] implementation of sequence and traverse methods" should "work" in {
    def sequence[E, A](es: List[Either[E, A]]): Either[E, List[A]] =
      traverse(es)(identity)

    def traverse[E, A, B](as: List[A])(
        f: A => Either[E, B]
    ): Either[E, List[B]] = as match {
      case Nil            => Right(Nil)
      case ::(head, tail) => (f(head) map2 traverse(tail)(f))(_ :: _)
    }

    val le = List(Right(1), Right(2), Right(3))
    val ls = List(1, 2, 3)

    val c = new Checkpoint
    assert(traverse(ls)(v => Right(v + 1)) == Right(List(2, 3, 4)))
    assert(traverse(ls)(_ => Left("ouch")) == Left("ouch"))

    assert(sequence(le) == Right(ls))
    assert(sequence(le :+ Left("ouch")) == Left("ouch"))
    c.reportAll()
  }
}
