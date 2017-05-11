package quickcheck

import java.lang.Math.min

import org.scalacheck.Arbitrary._
import org.scalacheck.Gen._
import org.scalacheck.Prop._
import org.scalacheck._

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  lazy val genHeap: Gen[H] = for {
    i <- arbitrary[Int]
    h <- oneOf(const(empty), genHeap)
  } yield insert(i, h)

  implicit val heapGen = Arbitrary(genHeap)

  property("gen1") = forAll { (h: H) =>
    val m = if (isEmpty(h)) 0 else findMin(h)
    findMin(insert(m, h)) == m
  }

  property("Inserting two elements in empty heap should return smallest") = forAll { (i1: Int, i2: Int) =>
    val h = insert(i1, insert(i2, empty))
    findMin(h) == min(i1, i2)
  }

  property("Insert element into empty heap and deleting smallest must result in empty heap") = forAll { (i: Int) =>
    isEmpty(deleteMin(insert(i, empty)))
  }

  property("Any heap must be sorted when finding minimum and deleting it") = forAll { (h: H) =>
    // TODO
  }

  property("FindMin of melded heaps must return minimum of one or the other") = forAll { (h1: H, h2: H) =>
    val meldedHeap = meld(h1, h2)

    findMin(meldedHeap) == min(findMin(h1), findMin(h2))
  }
}
