package patmat

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import patmat.Huffman._

@RunWith(classOf[JUnitRunner])
class HuffmanSuite extends FunSuite {
	trait TestTrees {
		val t1 = Fork(Leaf('a',2), Leaf('b',3), List('a','b'), 5)
		val t2 = Fork(Fork(Leaf('a',2), Leaf('b',3), List('a','b'), 5), Leaf('d',4), List('a','b','d'), 9)
	}


  test("weight of a larger tree") {
    new TestTrees {
      assert(weight(t1) === 5)
    }
  }



  test("chars of a larger tree") {
    new TestTrees {
      assert(chars(t2) === List('a','b','d'))
    }
  }


  test("string2chars(\"hello, world\")") {
    assert(string2Chars("hello, world") === List('h', 'e', 'l', 'l', 'o', ',', ' ', 'w', 'o', 'r', 'l', 'd'))
  }

  test("times function") {
    assert(times(List('a','b','d', 'a', 'd')) == List(('d', 2), ('b', 1), ('a', 2)))
  }


  test("makeOrderedLeafList for some frequency table") {
    assert(makeOrderedLeafList(List(('t', 2), ('e', 1), ('x', 3))) === List(Leaf('e',1), Leaf('t',2), Leaf('x',3)))
  }


  test("combine of some leaf list") {
    val leaflist = List(Leaf('e', 1), Leaf('t', 2), Leaf('x', 4))
    assert(combine(leaflist) === List(Fork(Leaf('e',1),Leaf('t',2),List('e', 't'),3), Leaf('x',4)))
  }

  test("createCodeTree works") {
    val tree = Fork(Leaf('e',1),Leaf('t',2),List('e', 't'),3)
    val chars = List('e', 't', 't')
    assert(createCodeTree(chars) == tree)
  }

  test("traverse_tree works") {
    val leaflist = List(Leaf('e', 1), Leaf('t', 2), Leaf('x', 4))
    val tree = combine(combine(leaflist)).head
    assert(traverse_tree(tree, List(1, 0, 0)) === ('x', List(0, 0)))
    assert(traverse_tree(tree, List(0, 0, 0)) === ('e', List(0)))
    assert(traverse_tree(tree, List(0, 0)) === ('e', List()))
    assert(traverse_tree(tree, List(0, 1, 1, 1)) === ('t', List(1, 1)))
  }

  test("decode works") {
    val leaflist = List(Leaf('e', 1), Leaf('t', 2), Leaf('x', 4))
    val tree = combine(combine(leaflist)).head

    assert(decode(tree, List(0, 1, 0, 0, 1, 0, 1)) === List('t', 'e', 'x', 't'))
  }

  test("search tree works") {
    val leaflist = List(Leaf('e', 1), Leaf('t', 2), Leaf('x', 4))
    val tree = combine(combine(leaflist)).head

    assert(searchTree(tree)('e', List()) === List(0, 0))
    assert(searchTree(tree)('t', List()) === List(0, 1))
    assert(searchTree(tree)('x', List()) === List(1))
  }

  test("decode and encode a very short text should be identity") {
    new TestTrees {
      assert(decode(t1, encode(t1)("ab".toList)) === "ab".toList)
    }
  }

  test("convert to code table works") {
    val leaflist = List(Leaf('e', 1), Leaf('t', 2), Leaf('x', 4))
    val tree = combine(combine(leaflist)).head

    assert(convert(tree) === List(('e',List(0, 0)), ('t',List(0, 1)), ('x',List(1))))
  }

  test("quick encode works") {
    new TestTrees {
      assert(decode(t1, quickEncode(t1)("ab".toList)) === "ab".toList)
    }
  }
}