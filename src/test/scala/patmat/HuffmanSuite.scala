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

  test("Char frequency") {
    new TestTrees {
      assert(times(string2Chars("aaabbc")) == List(('a',3), ('b',2), ('c',1)))
    }
  }


  test("string2chars(\"hello, world\")") {
    assert(string2Chars("hello, world") === List('h', 'e', 'l', 'l', 'o', ',', ' ', 'w', 'o', 'r', 'l', 'd'))
  }


  test("makeOrderedLeafList for some frequency table") {
    assert(makeOrderedLeafList(List(('t', 2), ('e', 1), ('x', 3))) === List(Leaf('e',1), Leaf('t',2), Leaf('x',3)))
  }


  test("combine of some leaf list") {
    val leaflist = List(Leaf('e', 1), Leaf('t', 2), Leaf('x', 4))
    assert(combine(leaflist) === List(Fork(Leaf('e',1),Leaf('t',2),List('e', 't'),3), Leaf('x',4)))
  }

  test("Until only one tree") {
    val leafList = makeOrderedLeafList(times("hello my name is Ollie".toList))
    val oneTree = until(singleton,combine)(leafList)
    assert(oneTree.length == 1)
  }


  test("decode and encode a very short text should be identity") {
    new TestTrees {
      assert(decode(t1, encode(t1)("abbba".toList)) === "abbba".toList)
    }
  }

  test("Decode the secret message") {
    println(decodedSecret.foldRight("")((x: Char, y: String) => x+y))
  }


  test("Convert a CodeTree into a CodeTable") {
    val tree = Fork(Fork(Leaf('a',1),Leaf('y', 2), List('a','y'), 3),Fork(Leaf('b',33), Leaf('c',45), List('b', 'c'), 33+45), List('a', 'y', 'b', 'c'), 33+45+1+2)

    val codeTable = convert(tree)

    assert(codeTable.contains(('a', List(0,0))))
    assert(codeTable.contains(('y', List(0,1))))
    assert(codeTable.contains(('b', List(1,0))))
    assert(codeTable.contains(('c', List(1,1))))

  }

  test("QuickEncode encodes a message using a CodeTable") {
    new TestTrees {
      assert(decode(t1, quickEncode(t1)("abbba".toList)) === "abbba".toList)
    }
  }

}
