package it.polentino.redbook

import org.scalatest.Checkpoints
import org.scalatest.flatspec.AnyFlatSpec

class Chapter03Spec extends AnyFlatSpec with Checkpoints {
  // List definition for the exercises
  sealed trait List[+A]
  final case object Nil extends List[Nothing]
  final case class Cons[+A](head: A, tail: List[A]) extends List[A]

  final case class NotAllowed(fnName: String)
      extends Exception(s"not allowed: $fnName of emtpy list")

  object List {
    def apply[A](as: A*): List[A] =
      if (as.isEmpty) Nil else Cons(as.head, apply(as.tail: _*))
  }

  // some utils
  def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B = as match {
    case Nil              => z
    case Cons(head, tail) => f(head, foldRight(tail, z)(f))
  }

  // EX 3.10
  def foldLeft[A, B](as: List[A], z: B)(f: (B, A) => B): B = as match {
    case Nil              => z
    case Cons(head, tail) => foldLeft(tail, f(z, head))(f)
  }

  private val threeInts: List[Int] = List(1, 2, 3)

  "[EX 3.2] implementation of the function tail" should "work" in {
    def tail[A](l: List[A]): List[A] = l match {
      case Nil           => throw NotAllowed("tail")
      case Cons(_, tail) => tail
    }

    val c = new Checkpoint
    assertThrows[NotAllowed](tail(Nil))
    assert(tail(threeInts) == List(2, 3))
    c.reportAll()
  }

  "[EX 3.3] implementation of the function setHead" should "work" in {
    def setHead[A](a: A, l: List[A]): List[A] = l match {
      case Nil           => throw NotAllowed("setHead")
      case Cons(_, tail) => Cons(a, tail)
    }

    val c = new Checkpoint
    assertThrows[NotAllowed](setHead(1, Nil))
    assert(setHead(9, List(1, 2)) == List(9, 2))
    c.reportAll()
  }

  "[EX 3.4] implementation of the function drop" should "work" in {
    def drop[A](l: List[A], n: Int): List[A] = l match {
      case _ if n <= 0             => l
      case Cons(_, tail) if n == 1 => tail
      case Cons(_, tail)           => drop(tail, n - 1)
      case Nil                     => Nil
    }

    val c = new Checkpoint
    assert(drop(Nil, -1) == Nil)
    assert(drop(Nil, 2) == Nil)
    assert(drop(threeInts, -1) == threeInts)
    assert(drop(threeInts, 1) == List(2, 3))
    assert(drop(threeInts, 2) == List(3))
    assert(drop(threeInts, 3) == Nil)
    c.reportAll()
  }

  "[EX 3.5] implementation of the function dropWhile" should "work" in {
    def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
      case Cons(head, tail) if f(head) => dropWhile(tail, f)
      case _                           => l
    }
    def dropWhile2[A](l: List[A])(f: A => Boolean): List[A] = l match {
      case Cons(head, tail) if f(head) => dropWhile2(tail)(f)
      case _                           => l
    }

    val c = new Checkpoint
    assert(dropWhile[Int](Nil, _ > 1) == Nil)
    assert(dropWhile[Int](threeInts, _ <= 1) == List(2, 3))
    assert(dropWhile[Int](threeInts, _ <= 2) == List(3))
    assert(dropWhile[Int](threeInts, _ <= 3) == Nil)
    assert(dropWhile2[Int](Nil)(_ > 1) == Nil)
    // currying helps type inference
    assert(dropWhile2(threeInts)(_ <= 1) == List(2, 3))
    assert(dropWhile2(threeInts)(_ <= 2) == List(3))
    assert(dropWhile2(threeInts)(_ <= 3) == Nil)
    c.reportAll()
  }

  "[EX 3.6] implementation of the function init" should "work" in {
    def init[A](l: List[A]): List[A] = l match {
      case Nil              => throw NotAllowed("init")
      case Cons(_, Nil)     => Nil
      case Cons(head, tail) => Cons(head, init(tail))
    }

    val c = new Checkpoint
    assertThrows[NotAllowed](init(Nil))
    assert(init(threeInts) == List(1, 2))
    assert(init(init(threeInts)) == List(1))
    c.reportAll()
  }

  "[EX 3.9] implementation of the function length" should "work" in {
    def length[A](l: List[A]): Int =
      foldRight(l, 0)((_, counter) => counter + 1)

    val c = new Checkpoint
    assert(length(Nil) == 0)
    assert(length(threeInts) == 3)
    c.reportAll()
  }

  "[EX 3.11] implementation of the function sum, product and length using foldLeft" should "work" in {
    def sum(l: List[Int]): Int = foldLeft(l, 0)(_ + _)
    def product(l: List[Double]): Double = foldLeft(l, 1d)(_ * _)
    def length[A](l: List[A]): Int = foldLeft(l, 0)((counter, _) => counter + 1)

    val c = new Checkpoint
    assert(sum(Nil) == 0)
    assert(sum(threeInts) == 6)
    assert(product(Nil) == 1.0)
    assert(product(List(2.0, 5.0)) == 10.0)
    assert(length(Nil) == 0)
    assert(length(threeInts) == 3)
    c.reportAll()
  }

  "[EX 3.12] implementation of the function reverse" should "work" in {
    def reverse[A](l: List[A]) =
      foldLeft(l, List[A]())((acc, h) => Cons(h, acc))

    val c = new Checkpoint
    assert(reverse(Nil) == Nil)
    assert(reverse(threeInts) == List(3, 2, 1))
    c.reportAll()
  }

  "[EX 3.13] implementation of the functions foldLeft using foldRight and vice-versa" should "work" in {
    def foldLeft1[A, B](as: List[A], z: B)(f: (B, A) => B): B =
      foldRight(as, (b: B) => b)((a, bFn) => b => f(bFn(b), a))(z)

    def foldRight1[A, B](as: List[A], z: B)(f: (A, B) => B): B =
      foldLeft(as, (b: B) => b)((bFn, a) => b => f(a, bFn(b)))(z)

    def sum1(l: List[Int]): Int = foldLeft1(l, 0)(_ + _)
    def product1(l: List[Double]): Double = foldLeft1(l, 1d)(_ * _)
    def length1[A](l: List[A]): Int =
      foldLeft1(l, 0)((counter, _) => counter + 1)

    def sum2(l: List[Int]): Int = foldRight1(l, 0)(_ + _)
    def product2(l: List[Double]): Double = foldRight1(l, 1d)(_ * _)
    def length2[A](l: List[A]): Int =
      foldRight1(l, 0)((_, counter) => counter + 1)

    val c = new Checkpoint
    assert(sum1(Nil) == 0)
    assert(sum1(threeInts) == 6)
    assert(product1(Nil) == 1.0)
    assert(product1(List(2.0, 5.0)) == 10.0)
    assert(length1(Nil) == 0)
    assert(length1(threeInts) == 3)

    assert(sum2(Nil) == 0)
    assert(sum2(threeInts) == 6)
    assert(product2(Nil) == 1.0)
    assert(product2(List(2.0, 5.0)) == 10.0)
    assert(length2(Nil) == 0)
    assert(length2(threeInts) == 3)
    c.reportAll()
  }

  "[EX 3.14] implementation of the function append" should "work" in {
    def append[A](l1: List[A], l2: List[A]): List[A] =
      foldRight(l1, l2)(Cons.apply)

    val c = new Checkpoint
    assert(append(threeInts, List(4)) == List(1, 2, 3, 4))
    assert(append(List(0), threeInts) == List(0, 1, 2, 3))
    assert(append(List(1, 2), List(3, 4, 5)) == List(1, 2, 3, 4, 5))
    c.reportAll()
  }

  "[EX 3.15] implementation of the function concat" should "work" in {
    def append[A](l1: List[A], l2: List[A]): List[A] =
      foldRight(l1, l2)(Cons.apply)

    def concat1[A](ls: List[List[A]]): List[A] = {
      def loop(head: List[A], tail: List[List[A]]): List[A] = tail match {
        case Nil                          => head
        case Cons(h1, Nil) if head == Nil => h1
        case Cons(h1, Nil)                => append(head, h1)
        case Cons(h1, t1) if head == Nil  => loop(append(head, h1), t1)
        case Cons(h1, t1)                 => loop(append(head, h1), t1)
      }

      ls match {
        case Nil              => Nil
        case Cons(head, Nil)  => head
        case Cons(head, tail) => loop(head, tail)
      }
    }

    def concat2[A](ls: List[List[A]]): List[A] =
      foldRight[List[A], List[A]](ls, Nil)(append)

    val l0 = List(0)
    val l1 = threeInts
    val l2 = List(4, 5)
    val l3 = List(6)
    val ls = List(l0, l1, l2, l3)

    val expected = List(0, 1, 2, 3, 4, 5, 6)

    val c = new Checkpoint
    assert(concat1(Nil) == Nil)
    assert(concat1(Cons(Nil, Cons(l1, Nil))) == threeInts)
    assert(concat1(Cons(threeInts, Nil)) == threeInts)
    assert(concat1(ls) == expected)

    assert(concat2(Nil) == Nil)
    assert(concat2(List(Nil, l1)) == threeInts)
    assert(concat2(List(threeInts, Nil)) == threeInts)
    assert(concat2(ls) == expected)
    c.reportAll()
  }

  "[EX 3.16] implementation of the function plusOne" should "work" in {
    def plusOne(l1: List[Int]): List[Int] =
      foldRight[Int, List[Int]](l1, Nil)((a, b) => Cons(a + 1, b))

    assert(plusOne(threeInts) == List(2, 3, 4))
  }

  "[EX 3.17] implementation of the function toString" should "work" in {
    def toString(l1: List[Int]): List[String] =
      foldRight[Int, List[String]](l1, Nil)((a, b) => Cons(a.toString, b))

    assert(toString(threeInts) == List("1", "2", "3"))
  }

  "[EX 3.18] implementation of the function map" should "work" in {
    def map[A, B](as: List[A])(f: A => B): List[B] =
      foldRight[A, List[B]](as, Nil)((a, b) => Cons(f(a), b))

    val c = new Checkpoint
    assert(map(threeInts)(_ + 1) == List(2, 3, 4))
    assert(map(threeInts)(_.toString) == List("1", "2", "3"))
    c.reportAll()
  }

  "[EX 3.19] implementation of the function filter" should "work" in {
    def filter1[A](as: List[A])(f: A => Boolean): List[A] = as match {
      case Nil                         => Nil
      case Cons(head, tail) if f(head) => Cons(head, filter1(tail)(f))
      case Cons(_, tail)               => filter1(tail)(f)
    }

    def filter2[A](as: List[A])(f: A => Boolean): List[A] =
      foldRight[A, List[A]](as, Nil)((a, b) => if (f(a)) Cons(a, b) else b)

    val c = new Checkpoint
    assert(filter1(threeInts)(_ % 2 == 0) == List(2))
    assert(filter1(threeInts)(_ % 2 != 0) == List(1, 3))

    assert(filter2(threeInts)(_ % 2 == 0) == List(2))
    assert(filter2(threeInts)(_ % 2 != 0) == List(1, 3))
    c.reportAll()
  }

  "[EX 3.20] implementation of the function flatMap" should "work" in {
    def append[A](l1: List[A], l2: List[A]): List[A] =
      foldRight(l1, l2)(Cons.apply)

    def flatMap[A, B](as: List[A])(f: A => List[B]): List[B] =
      foldRight[A, List[B]](as, Nil)((a, b) => append(f(a), b))

    val c = new Checkpoint
    assert(flatMap[Int, Int](Nil)(i => List(i, i)) == Nil)
    assert(flatMap(threeInts)(i => List(i, i)) == List(1, 1, 2, 2, 3, 3))
    c.reportAll()
  }

  "[EX 3.21] implementation of the function filter using flatMap" should "work" in {
    def append[A](l1: List[A], l2: List[A]): List[A] =
      foldRight(l1, l2)(Cons.apply)

    def flatMap[A, B](as: List[A])(f: A => List[B]): List[B] =
      foldRight[A, List[B]](as, Nil)((a, b) => append(f(a), b))

    def filter[A](as: List[A])(f: A => Boolean): List[A] =
      flatMap(as)(a => if (f(a)) List(a) else Nil)

    val c = new Checkpoint
    assert(filter(threeInts)(_ % 2 == 0) == List(2))
    assert(filter(threeInts)(_ % 2 != 0) == List(1, 3))
    c.reportAll()
  }

  "[EX 3.22] implementation of the function sum of two lists" should "work" in {
    def sum(as: List[Int], bs: List[Int]): List[Int] = (as, bs) match {
      case (Cons(h1, t1), Cons(h2, t2)) => Cons(h1 + h2, sum(t1, t2))
      case (c, Nil)                     => c
      case (Nil, c)                     => c
      case _                            => Nil
    }

    val c = new Checkpoint
    assert(sum(threeInts, threeInts) == List(2, 4, 6))
    assert(sum(threeInts, List(4)) == List(5, 2, 3))
    c.reportAll()
  }

  "[EX 3.23] implementation of the function zipWith" should "work" in {
    def zipWith[A](as: List[A], bs: List[A])(f: (A, A) => A): List[A] =
      (as, bs) match {
        case (Cons(h1, t1), Cons(h2, t2)) => Cons(f(h1, h2), zipWith(t1, t2)(f))
        case (c, Nil)                     => c
        case (Nil, c)                     => c
        case _                            => Nil
      }

    val c = new Checkpoint
    assert(zipWith(threeInts, threeInts)(_ + _) == List(2, 4, 6))
    assert(zipWith(threeInts, List(4))(_ + _) == List(5, 2, 3))
    c.reportAll()
  }

  "[EX 3.25] implementation of the function hasSubsequence" should "work" in {
    def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean = {
      def startsWith(l1: List[A], l2: List[A]): Boolean = (l1, l2) match {
        case (_, Nil)                                 => true
        case (Cons(h1, t1), Cons(h2, t2)) if h1 == h2 => startsWith(t1, t2)
        case _                                        => false
      }

      def loop(sup: List[A]): Boolean = sup match {
        case _ if startsWith(sup, sub) => true
        case Cons(_, tail)             => loop(tail)
        case _                         => false
      }

      loop(sup)
    }

    val c = new Checkpoint
    // degenerate cases
    assert(hasSubsequence(Nil, Nil))
    assert(hasSubsequence(threeInts, Nil))
    assert(!hasSubsequence(Nil, threeInts))
    assert(!hasSubsequence(List(1), List(2)))
    assert(hasSubsequence(List(1), List(1)))
    //
    assert(hasSubsequence(threeInts, List(1)))
    assert(hasSubsequence(threeInts, List(2)))
    assert(hasSubsequence(threeInts, List(3)))
    //
    assert(hasSubsequence(List(0, 1, 2, 3, 4), threeInts))
    assert(hasSubsequence(List(1, 1, 2, 3, 4, 5), threeInts))
    assert(hasSubsequence(List(1, 1, 1, 2, 3, 4, 5), threeInts))
    assert(!hasSubsequence(List(1, 2, 3, 4, 4, 5, 5, 6, 7, 8), List(4, 5, 6)))
    c.reportAll()
  }
}
