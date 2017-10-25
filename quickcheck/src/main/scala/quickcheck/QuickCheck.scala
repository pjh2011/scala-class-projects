package quickcheck

import common._

import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  lazy val genHeap: Gen[H] = oneOf(
    const(empty),
    for {
      k <- arbitrary[Int]
      m <- oneOf(const(empty), genHeap)
    } yield insert(k, m)
  )

  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

  property("gen1") = forAll { (h: H) =>
    val m = if (isEmpty(h)) 0 else findMin(h)
    findMin(insert(m, h)) == m
  }

  // if heap is empty, merge with another heap and ensure you get back the min from that heap
  property("merge2_compare_mins") = forAll { (h1: H, h2: H) =>
    val m1 = if (isEmpty(h1)) Int.MaxValue else findMin(h1)
    val m2 = if (isEmpty(h2)) Int.MaxValue else findMin(h2)

    val h3 = meld(h1, h2)
    val m3 = if (isEmpty(h3)) Int.MaxValue else findMin(h3)

    m3 == List(m1, m2).min

  }

  def empty_heap(h: H, acc: List[Int] = List()): (H, List[Int]) = {
    if (isEmpty(h)) (h, acc)
    else {
      val new_ele = findMin(h)
      val h_min_removed = deleteMin(h)

      empty_heap(h_min_removed, new_ele :: acc)
    }
  }

  def insert_list(h: H, ints: List[Int]): H = {
    if (ints.isEmpty) h
    else {
      insert_list(insert(ints.head, h), ints.tail)
    }
  }

  // if empty pass, otherwise fill with random ints then empty is and make sure you get back same thing
  property("fill_and_empty_ensure_same") = forAll { (h: H) =>
    val r = scala.util.Random
    val rand_ints = (for (i <- 1 to 5) yield r.nextInt(2000) - 1000).toList

    if (!isEmpty(h)) true
    else {

      val h_new = insert_list(h, rand_ints)

      val (_, heap_emptied) = empty_heap(h_new)

      heap_emptied == rand_ints.sortWith(_ > _)
    }
  }


  // meld two heaps and empty, ensure it returns the same as the individual heaps emptied then merged
  property("fill_and_empty_and_meld_ensure_same") = forAll { (h1: H, h2: H) =>
    val r = scala.util.Random

    if (!(isEmpty(h1) & isEmpty(h2))) true
    else {
      // generate two random in lists
      val rand_ints1 = (for (i <- 0 to r.nextInt(1000)) yield r.nextInt(2000) - 1000).toList
      val rand_ints2 = (for (i <- 0 to r.nextInt(1000)) yield r.nextInt(2000) - 1000).toList

      // insert each int list into the heaps
      val h1_new = insert_list(h1, rand_ints1)
      val h2_new = insert_list(h2, rand_ints2)

      // meld them
      val h_meld = meld(h1_new, h2_new)

      // empty it out
      val (_, heap_emptied) = empty_heap(h_meld)

      // make sure that the emptied list is the same as the two input lists concatenated then ordered
      heap_emptied == (rand_ints1 ++ rand_ints2).sortWith(_ > _)
    }
  }
}
