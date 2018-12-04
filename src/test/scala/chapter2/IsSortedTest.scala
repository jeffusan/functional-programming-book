package chapter2

import org.scalatest._

trait Sorter {

  def intSorted(a: Int, b: Int): Boolean = ( a <= b )

  def isSorted[A](as: Array[A], ordered: (A, A) => Boolean): Boolean = {
      val b = as.toSeq.sliding(2) // get pairwise
      val c = b.map( pair => ordered(pair.head, pair.last)) // apply predicate
      c.foldLeft(true)(_ && _) // fold over results
  }

}

class IsSortedTest extends FlatSpec with Matchers  with Sorter {

  "a sorted array" should "be detected" in {
    isSorted(Array[Int](1,2,3,4), intSorted) shouldBe true
  }

  "an unsorted array" should "be detected" in {
    isSorted(Array[Int](2,4,1,65), intSorted) shouldBe false
  }

  "a single element" should "be considered sorted" in {
    isSorted(Array[Int](1), intSorted) shouldBe true
  }
}
