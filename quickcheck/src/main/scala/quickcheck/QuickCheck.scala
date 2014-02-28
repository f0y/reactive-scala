package quickcheck

import common._
import java.lang.Math._


import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  property("min1") = forAll { a: Int =>
    val h = insert(a, empty)
    findMin(h) == a
  }
  property("test 1") = forAll { par: (Int, Int) =>
    val h = insert(par._2, insert(par._1, empty))
    findMin(h) == min(par._1, par._2)
  }

  property("test 2") = forAll { par: (Int, Int) =>
    val h = insert(par._2, insert(par._1, empty))
    val c = deleteMin(h)
    findMin(c) == max(par._1, par._2)
  }

  property("test 3") = forAll { num: List[Int] =>
    val h = num.foldLeft(empty)((h: H, n: Int) => insert(n, h))
    val mustBeSorted = makeList(h)
    mustBeSorted == num.sorted
  }

  property("test 4") = forAll { (h1: H, h2: H) =>
    val merged = meld(h1, h2)
    findMin(merged) == min(findMin(h1), findMin(h2))
  }

  def makeList(heap: H): List[Int] = {
    if (isEmpty(heap)) Nil else findMin(heap) :: makeList(deleteMin(heap))
  }

  lazy val genHeap: Gen[H] = for {
    rl <- arbitrary[Int]
    v <- oneOf(value(empty), genHeap)
  } yield insert(rl, v)

  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

}
