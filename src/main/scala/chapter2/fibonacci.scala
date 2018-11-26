package chapter2

import annotation.tailrec

object Fibonacci {

  def fib(n: Int): Int = {

    @tailrec
    def fib0(one: Int, two: Int, i: Int): Int =
      if (i == n) one + two
      else fib0(two, one + two, i + 1)

    if (n <= 1) n
    else fib0(0, 1, 2)
  }
}
