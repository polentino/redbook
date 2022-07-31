package it.polentino.redbook

import org.scalatest.Checkpoints
import org.scalatest.flatspec.AnyFlatSpec

class Chapter02Spec extends AnyFlatSpec with Checkpoints {

  "[EX 2.1] write recursive function to get the n-th Fibonacci number" should "work" in {
    def fib(n: Int): Int = n match {
      case 0 => 0
      case 1 => 1
      case _ => fib(n - 1) + fib(n - 2)
    }
    assert(fib(10) == 55)
  }

  "[EX 2.2] implement isSorted" should "work" in {
    def isSorted[A](as: Array[A], ordered: (A, A) => Boolean): Boolean =
      as.sliding(2).forall { case Array(first, second) =>
        ordered(first, second)
      }

    def s = (a: Int, b: Int) => a < b

    val c = new Checkpoint
    assert(isSorted(Array(1, 2, 3, 4), s))
    assert(!isSorted(Array(1, 2, 3, 4, 2), s))
    assert(!isSorted(Array(1, 4, 6, 2, 7), s))
    c.reportAll()
  }

  "[EX 2.3] implementation of curry function" should "compile" in {
    assertCompiles("""def curry[A,B,C](f: (A,B) => C): A => (B => C) =
        |   (a: A) => ((b: B) => f(a,b))""".stripMargin)
  }

  "[EX 2.4] implementation of uncurry function" should "compile" in {
    assertCompiles("""def uncurry[A,B,C](f: A => B => C): (A, B) => C =
        |   (a: A, b: B) => f(a)(b)""".stripMargin)
  }

  "[EX 2.5] implementation of compose function" should "compile" in {
    assertCompiles("""def compose[A,B,C](f: B => C, g: A => B): A => C =
        |   (a: A) => f(g(a))""".stripMargin)
  }
}
