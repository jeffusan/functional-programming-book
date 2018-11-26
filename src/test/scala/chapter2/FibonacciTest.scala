package chapter2

import org.scalatest._
import org.scalacheck.Gen
import org.scalatest.prop.PropertyChecks

class FibonacciTest extends PropSpec with Matchers with PropertyChecks {

  val fibs = Gen.oneOf (
        (0, 0),
        (1, 1),
        (2, 1),
        (3, 2),
        (4, 3),
        (5, 5),
        (6, 8),
        (7, 13),
        (8, 21),
        (9, 34),
        (10, 55),
        (11, 89),
        (12, 144),
        (13, 233),
        (14, 377),
        (15, 610),
        (16, 987),
        (17, 1597),
        (18, 2584)
  )

  property("fib function tail recursion") {
    forAll(fibs) { f =>
      Fibonacci.fib(f._1) shouldBe f._2
    }
  }
}