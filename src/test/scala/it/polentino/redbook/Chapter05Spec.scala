package it.polentino.redbook

import org.scalatest.{Checkpoints, color}
import org.scalatest.flatspec.AnyFlatSpec

class Chapter05Spec extends AnyFlatSpec with Checkpoints {
  sealed trait Stream[+A] {

    def toList: List[A] = {
      def loop(as: Stream[A], acc: List[A] = Nil): List[A] = as match {
        case Cons(h, t) => loop(t(), acc :+ h())
        case _          => acc
      }

      loop(this)
    }

    def take(n: Int): Stream[A] = this match {
      case Cons(h, t) if n > 0 => Stream.cons(h(), t().take(n - 1))
      case _                   => Empty
    }

    def foldRight[B](z: => B)(f: (A, => B) => B): B = this match {
      case Cons(h, t) => f(h(), t().foldRight(z)(f))
      case _          => z
    }

    def exists(p: A => Boolean): Boolean = foldRight(false)((a, b) => p(a) || b)
  }

  case object Empty extends Stream[Nothing]
  case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

  object Stream {
    def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
      lazy val h = hd
      lazy val t = tl
      Cons(() => h, () => t)
    }

    def empty[A]: Stream[A] = Empty

    def apply[A](as: A*): Stream[A] =
      if (as.isEmpty) empty else cons(as.head, apply(as.tail: _*))

    def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] =
      f(z) match {
        case None         => Stream.empty[A]
        case Some((a, s)) => Stream.cons(a, unfold(s)(f))
      }
  }

  "[EX 5.1] write a function to convert a stream into a list" should "work" in {
    val c = new Checkpoint
    assert(Empty.toList == Nil)
    assert(Stream(1, 2, 3).toList == List(1, 2, 3))
    c.reportAll()
  }

  "[EX 5.2] write take(n) and drop(n) functions" should "work" in {

    def drop[A](s: Stream[A], n: Int): Stream[A] = s match {
      case Cons(_, t) if n > 0  => drop(t(), n - 1)
      case c: Cons[A] if n == 0 => c
      case _                    => Empty
    }

    val c = new Checkpoint
    assert(Empty.take(0).toList == Nil)
    assert(Empty.take(2).toList == Nil)
    assert(Stream(1, 2, 3).take(0).toList == Nil)
    assert(Stream(1, 2, 3).take(1).toList == List(1))
    assert(Stream(1, 2, 3).take(2).toList == List(1, 2))
    assert(Stream(1, 2, 3).take(8).toList == List(1, 2, 3))
    //
    assert(drop(Empty, 0).toList == Nil)
    assert(drop(Empty, 2).toList == Nil)
    assert(drop(Stream(1, 2, 3), 0).toList == List(1, 2, 3))
    assert(drop(Stream(1, 2, 3), 1).toList == List(2, 3))
    assert(drop(Stream(1, 2, 3), 2).toList == List(3))
    assert(drop(Stream(1, 2, 3), 8).toList == Nil)
    c.reportAll()
  }

  "[EX 5.3] write takeWhile function" should "work" in {
    def takeWhile[A](s: Stream[A], p: A => Boolean): Stream[A] = s match {
      case Cons(h, t) if p(h()) => Stream.cons(h(), takeWhile(t(), p))
      case _                    => Empty
    }

    val c = new Checkpoint
    assert(takeWhile[Int](Empty, _ % 2 == 0).toList == Nil)
    assert(takeWhile[Int](Stream(1, 2, 3), _ % 2 == 0).toList == Nil)
    assert(takeWhile[Int](Stream(1, 2, 3), _ % 2 != 0).toList == List(1))
    assert(takeWhile[Int](Stream(1, 2, 3), _ < 3).toList == List(1, 2))
    assert(takeWhile[Int](Stream(1, 2, 3), _ > 3).toList == Nil)
    c.reportAll()
  }

  "[EX 5.4] write forAll function" should "work" in {
    def forAll[A](s: Stream[A], p: A => Boolean): Boolean =
      s.foldRight(true)((a, b) => p(a) && b)

    val c = new Checkpoint
    assert(forAll[Int](Empty, _ == 2))
    assert(!forAll[Int](Stream(1), _ == 2))
    assert(forAll[Int](Stream(1, 2, 3), _ < 4))
    assert(!forAll[Int](Stream(1, 2, 3), _ > 4))
    c.reportAll()
  }

  "[EX 5.5] implementation of takeWhile with foldRight" should "work" in {
    def takeWhile[A](s: Stream[A], p: A => Boolean): Stream[A] =
      s.foldRight(Empty.asInstanceOf[Stream[A]]) { (a, b) =>
        if (p(a)) Stream.cons(a, b)
        else Empty
      }

    val c = new Checkpoint
    assert(takeWhile[Int](Empty, _ % 2 == 0).toList == Nil)
    assert(takeWhile[Int](Stream(1, 2, 3), _ % 2 == 0).toList == Nil)
    assert(takeWhile[Int](Stream(1, 2, 3), _ % 2 != 0).toList == List(1))
    assert(takeWhile[Int](Stream(1, 2, 3), _ < 3).toList == List(1, 2))
    assert(takeWhile[Int](Stream(1, 2, 3), _ > 3).toList == Nil)
    c.reportAll()
  }

  "[EX 5.6][hard] implementation of headOption with foldRight" should "work" in {
    def headOption[A](s: Stream[A]): Option[A] =
      s.foldRight(Option.empty[A]) { (a, _) => Option(a) }

    val c = new Checkpoint
    assert(headOption[Int](Empty).isEmpty)
    assert(headOption[Int](Stream(1)).contains(1))
    assert(headOption[Int](Stream(1, 2, 3)).contains(1))
    c.reportAll()
  }

  "[EX 5.7] implement map, filter, flatMap and append with foldRight" should "work" in {
    def map[A, B](s: Stream[A], f: A => B): Stream[B] =
      s.foldRight(Empty.asInstanceOf[Stream[B]]) { (a, b) =>
        Stream.cons(f(a), b)
      }

    def filter[A](s: Stream[A], f: A => Boolean): Stream[A] =
      s.foldRight(Empty.asInstanceOf[Stream[A]]) { (a, b) =>
        if (f(a)) Stream.cons(a, b) else b
      }

    def append[A](s: Stream[A], t: Stream[A]): Stream[A] =
      s.foldRight(t)(Stream.cons(_, _))

    def flatMap[A, B](s: Stream[A], f: A => Stream[B]): Stream[B] =
      s.foldRight(Empty.asInstanceOf[Stream[B]]) { (a, b) =>
        append(f(a), b)
      }

    val c = new Checkpoint
    assert(map[Int, Int](Empty, _ * 2) == Empty)
    assert(map[Int, Int](Stream(1, 2, 3), _ * 2).toList == List(2, 4, 6))
    //
    assert(filter[Int](Empty, _ % 2 == 0) == Empty)
    assert(filter[Int](Stream(1, 2, 3, 4), _ % 2 == 0).toList == List(2, 4))
    //
    assert(append[Int](Empty, Empty).toList == Nil)
    assert(append[Int](Empty, Stream(1, 2, 3)).toList == List(1, 2, 3))
    assert(append[Int](Stream(1, 2, 3), Empty).toList == List(1, 2, 3))
    assert(
      append[Int](Stream(-2, -1, 0), Stream(1, 2, 3)).toList == List(-2, -1, 0,
        1, 2, 3)
    )
    //
    assert(flatMap[Int, Int](Empty, a => Stream(a * 2)) == Empty)
    assert(
      flatMap[Int, Int](Stream(1, 2, 3, 4), a => Stream(a * 2)).toList ==
        List(2, 4, 6, 8)
    )
    c.reportAll()
  }

  "[EX 5.8] implementation of constant" should "work" in {
    def take[A](s: Stream[A], n: Int): Stream[A] = s match {
      case Cons(h, t) if n > 0 => Stream.cons(h(), take(t(), n - 1))
      case _                   => Empty
    }

    def constant[A](a: A): Stream[A] = Stream.cons(a, constant(a))

    assert(take(constant[Int](1), 3).toList == List(1, 1, 1))
  }

  "[EX 5.9] implementation of from" should "work" in {
    def take[A](s: Stream[A], n: Int): Stream[A] = s match {
      case Cons(h, t) if n > 0 => Stream.cons(h(), take(t(), n - 1))
      case _                   => Empty
    }

    def from(a: Int): Stream[Int] = Stream.cons(a, from(a + 1))

    assert(take(from(1), 3).toList == List(1, 2, 3))
  }

  "[EX 5.10] implementation of fibs" should "work" in {
    def take[A](s: Stream[A], n: Int): Stream[A] = s match {
      case Cons(h, t) if n > 0 => Stream.cons(h(), take(t(), n - 1))
      case _                   => Empty
    }

    def fibs: Stream[Int] = {
      def loop(start: Int, next: Int): Stream[Int] =
        Stream.cons(start, loop(next, start + next))
      loop(0, 1)
    }

    assert(
      take(fibs, 10).toList ==
        List(0, 1, 1, 2, 3, 5, 8, 13, 21, 34)
    )
  }

  "[EX 5.11] implementation of unfold" should "work" in {
    assert(
      Stream.unfold[Int, Int](0)(v => Some(v, v + 1)).take(4).toList ==
        List(0, 1, 2, 3)
    )
  }

  "[EX 5.12] implementation of fibs, from, constant, ones using unfold" should "work" in {
    def fibs: Stream[Int] = Stream.unfold((0, 1)) { case (prev, current) =>
      Some((prev, (current, prev + current)))
    }

    def from(a: Int): Stream[Int] = Stream.unfold(a)(v => Some((v, v + 1)))

    def constant(a: Int): Stream[Int] = Stream.unfold(a)(_ => Some((a, a)))

    def ones: Stream[Int] = Stream.unfold(1)(v => Some((v, v)))

    val c = new Checkpoint
    assert(
      fibs.take(10).toList ==
        List(0, 1, 1, 2, 3, 5, 8, 13, 21, 34)
    )
    assert(
      Stream.unfold[Int, Int](0)(v => Some(v, v + 1)).take(4).toList ==
        List(0, 1, 2, 3)
    )
    assert(from(2).take(3).toList == List(2, 3, 4))
    assert(constant(2).take(3).toList == List(2, 2, 2))
    assert(ones.take(3).toList == List(1, 1, 1))
    c.reportAll()
  }

  "[EX 5.13] implementation of map, take, takeWhile, zipWith, zipAll using unfold" should "work" in {
    def map[A, B](as: Stream[A], f: A => B): Stream[B] = Stream.unfold(as) {
      case Cons(h, t) => Some(f(h()), t())
      case _          => None
    }

    def takeWithUnfold[A](as: Stream[A], n: Int) = Stream.unfold((as, n)) {
      case (Cons(h, t), n) if n > 0 => Some((h(), (t(), n - 1)))
      case _                        => None
    }

    def takeWhile[A](as: Stream[A], p: A => Boolean): Stream[A] =
      Stream.unfold(as) {
        case Cons(h, t) if p(h()) => Some(h(), takeWhile(t(), p))
        case _                    => None
      }

    def zipWith[A](as: Stream[A], bs: Stream[A])(f: (A, A) => A): Stream[A] =
      Stream.unfold((as, bs)) {
        case (Cons(h1, t1), Cons(h2, t2)) => Some((f(h1(), h2()), (t1(), t2())))
        case _                            => None
      }

    def zipAll[A, B](
        as: Stream[A],
        bs: Stream[B]
    ): Stream[(Option[A], Option[B])] = Stream.unfold((as, bs)) {
      case (Cons(h1, t1), Cons(h2, t2)) =>
        Some((Some(h1()), Some(h2())), (t1(), t2()))
      case (Empty, Cons(h2, t2)) =>
        Some((Option.empty[A], Some(h2())), (Stream.empty[A], t2()))
      case (Cons(h1, t1), Empty) =>
        Some((Some(h1()), Option.empty[B]), (t1(), Stream.empty[B]))
      case (Empty, Empty) => None
    }

    val threeInts = Stream(1, 2, 3)

    val c = new Checkpoint
    assert(map[Int, Int](Empty, _ * 2) == Empty)
    assert(map[Int, Int](Stream(1, 2, 3), _ * 2).toList == List(2, 4, 6))
    //
    assert(
      takeWithUnfold(
        Stream.unfold[Int, Int](0)(v => Some(v, v + 1)),
        4
      ).toList == List(0, 1, 2, 3)
    )
    //
    assert(takeWhile[Int](Empty, _ % 2 == 0).toList == Nil)
    assert(takeWhile[Int](Stream(1, 2, 3), _ % 2 == 0).toList == Nil)
    assert(takeWhile[Int](Stream(1, 2, 3), _ % 2 != 0).toList == List(1))
    assert(takeWhile[Int](Stream(1, 2, 3), _ < 3).toList == List(1, 2))
    assert(takeWhile[Int](Stream(1, 2, 3), _ > 3).toList == Nil)
    //
    assert(zipWith(threeInts, threeInts)(_ + _).toList == List(2, 4, 6))
    assert(zipWith(threeInts, Stream(4))(_ + _).toList == List(5))
    //
    assert(
      zipAll(threeInts, threeInts).toList == List(
        (Some(1), Some(1)),
        (Some(2), Some(2)),
        (Some(3), Some(3))
      )
    )
    assert(
      zipAll(threeInts, Stream(4)).toList == List(
        (Some(1), Some(4)),
        (Some(2), None),
        (Some(3), None)
      )
    )
    c.reportAll()
  }
}
