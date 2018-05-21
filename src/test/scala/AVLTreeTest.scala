import AVLNode._

import org.scalatest.FunSuite

class AVLTreeTest extends FunSuite{
  test("Two different nodes are not equal"){
    val t1 = AVLNode(6)
    val t2 = AVLNode(9)
    assert(t1 != t2)
  }

  test("Two nodes with the same value are equal"){
    val t1 = AVLNode(6)
    val t2 = AVLNode(6)
    assert(t1 == t2)
  }

  test("Value found if is present in single node"){
    val t1 = AVLNode(2)
    assert(search(2)(t1))
  }

  test("Value found if is present in multiple nodes tree"){
    val t1:AVLNode[Int] = AVLNode(1, AVLNode(0, AVLNode(-1), AVLNode(2)), AVLNode(3, AVLNode(4), AVLNode(5, AVLNode(6), AVLNode(7))))
    assert(search(7)(t1))
  }

  test("Value not found if not present"){
    val t1 = AVLNode(2)
    assert(!search(3)(t1))
  }

  test("Empty tree has height equal to 0"){
    val t1 = AVLNil
    assert(height(t1) == 0)
  }

  test("One-node tree has height equal to 1"){
    val t1 = AVLNode(1)
    assert(height(t1) == 1)
  }

  test("Multiple nodes tree has proper height"){
    val t1 = AVLNode(1, AVLNode(0, AVLNode(-1, AVLNil)), AVLNode(3, AVLNil, AVLNode(5, AVLNil, AVLNode(7))))
    assert(height(t1) == 4)
  }

  test("Tree has proper height after insertion"){
    val t1 = AVLNode(3)
    val t2 = insert(4)(t1)
    assert(height(t2) == 2)
  }

  test("Value present after insertion"){
    val t1 = AVLNode(3)
    val t2 = insert(4)(t1)
    assert(search(4)(t2))
  }

  test("Empty tree is balanced"){
    val t1 = AVLNil
    assert(getBalance(t1) == 0)
  }

  test("One-node tree is balanced"){
    val t1 = AVLNode(2)
    assert(getBalance(t1) == 0)
  }

  test("Node with left children is unbalanced"){
    val t1 = AVLNode(2, AVLNode(1, AVLNode(3), AVLNil), AVLNil)
    assert(getBalance(t1) > 1)
  }

  test("Node with right children is unbalanced"){
    val t1 = AVLNode(2, AVLNil, AVLNode(3, AVLNode(1), AVLNil))
    assert(getBalance(t1) < -1)
  }

  test("Exception is thrown after right rotation on too small tree"){
    val t1 = AVLNode(10)
    assertThrows[RuntimeException](rightRotate(t1))
  }

  test("Exception is thrown after left rotation on too small tree"){
    val t1 = AVLNode(10)
    assertThrows[RuntimeException](leftRotate(t1))
  }

  test("Right rotation performed correctly"){
    val tree = AVLNode(10, AVLNode(7, AVLNode(6), AVLNode(8)), AVLNode(12))

    val rotated = rightRotate(tree)
    val expected = AVLNode(7, AVLNode(6), AVLNode(10, AVLNode(8), AVLNode(12)))

    assert(rotated == expected)
  }

  test("Left rotation performed correctly"){
    val tree = AVLNode(7, AVLNode(6), AVLNode(10, AVLNode(8), AVLNode(12)))

    val rotated = leftRotate(tree)
    val expected = AVLNode(10, AVLNode(7, AVLNode(6), AVLNode(8)), AVLNode(12))

    assert(rotated == expected)
  }

  test("Tree after right rotation is balanced"){
    val tree = AVLNode(5, AVLNode(3, AVLNode(2), AVLNil), AVLNode(7))

    val rotated = rightRotate(tree)

    assert(getBalance(rotated) == -1)
  }

  test("Tree after left rotation is balanced"){
    val tree = AVLNode(5, AVLNode(3), AVLNode(3, AVLNil, AVLNode(4)))

    val rotated = leftRotate(tree)

    assert(getBalance(rotated) == 1)
  }

  test("Tree after several inserts is balanced"){
    val t1 = AVLNode(20)

    val t2 = insert(10)(t1)
    val t3 = insert(5)(t2)
    val t4 = insert(3)(t3)
    val t5 = insert(1)(t4)
    val t6 = insert(22)(t5)
    val t7 = insert(25)(t6)
    val t8 = insert(30)(t7)

    assert(getBalance(t8) == -1)
  }

  test("Maximum value is returned"){
    val tree = AVLNode(5, AVLNode(3), AVLNode(6))
    val tree2 = insert(10)(tree)

    val maxValue = getMaxValue(tree2)

    assert(maxValue == 10)
  }

  test("Minimum value is returned"){
    val tree = AVLNode(5, AVLNode(3), AVLNode(6, AVLNil, AVLNode(7)))
    val tree2 = insert(12)(tree)

    val minValue = getMinValue(tree2)

    assert(minValue == 3)
  }


  test("Node with one child is deleted"){
    val tree = AVLNode(5, AVLNode(3), AVLNode(6, AVLNil, AVLNode(7)))

    val treeDeleted = remove(6)(tree)

    assert(!search(6)(treeDeleted))
  }

  test("Root node is deleted"){
    val tree = AVLNode(5, AVLNode(3), AVLNode(6, AVLNil, AVLNode(7)))

    val treeDeleted = remove(5)(tree)

    assert(!search(5)(treeDeleted))
  }


  test("Node with no children is deleted"){
    val tree = AVLNode(5, AVLNode(3), AVLNode(6, AVLNil, AVLNode(7)))

    val treeDeleted = remove(7)(tree)

    assert(!search(7)(treeDeleted))
  }


  test("Tree is balanced after deletion of node with one child"){
    val tree = AVLNode(5, AVLNode(3), AVLNode(6, AVLNil, AVLNode(7)))

    val treeDeleted = remove(6)(tree)

    assert(math.abs(getBalance(treeDeleted)) <= 1)
  }

  test("Tree is balanced after deletion of root node"){
    val tree = AVLNode(5, AVLNode(3), AVLNode(6, AVLNil, AVLNode(7)))

    val treeDeleted = remove(5)(tree)

    assert(math.abs(getBalance(treeDeleted)) <= 1)
  }


  test("Tree is balanced after deletion of node with no children"){
    val tree = AVLNode(5, AVLNode(3), AVLNode(6, AVLNil, AVLNode(7)))

    val treeDeleted = remove(7)(tree)

    assert(math.abs(getBalance(treeDeleted)) <= 1)
  }

  test("Tree is the same after deletion value not present in original tree"){
    val tree = AVLNode(5, AVLNode(3), AVLNode(6, AVLNil, AVLNode(7)))

    val treeDeleted = remove(70)(tree)

    assert(tree == treeDeleted)
  }

  test("Union of two AVLNils is AVLNil"){
    val t1 = AVLNil
    val t2 = AVLNil

    assert(union(t1)(t2) == AVLNil)
  }

  test("Union of one tree and AVLNil is the first tree"){
    val t1 = AVLNode(7, AVLNode(1), AVLNode(8))
    val t2 = AVLNil

    assert(union(t1)(t2) == t1)
  }

  test("Union of two trees is computed properly"){
    val t1 = AVLNode(7, AVLNode(1), AVLNode(8))
    val t2 = AVLNode(6, AVLNode(0), AVLNode(9))

    val temp = AVLNil
    val temp1 = insert(7)(temp)
    val temp2 = insert(1)(temp1)
    val temp3 = insert(8)(temp2)
    val temp4 = insert(6)(temp3)
    val temp5 = insert(0)(temp4)
    val temp6 = insert(9)(temp5)

    assert(union(t1)(t2) == temp6)
  }

  test("Intersection of two AVLNils is AVLNil"){
    val t1 = AVLNil
    val t2 = AVLNil

    assert(intersect(t1)(t2) == AVLNil)
  }

  test("Intersection of one tree and AVLNil is AVLNil"){
    val t1 = AVLNode(7, AVLNode(1), AVLNode(8))
    val t2 = AVLNil

    assert(intersect(t1)(t2) == AVLNil)
  }

  test("Intersection of separate trees is AVLNil"){
    val t1 = AVLNode(7, AVLNode(1), AVLNode(8))
    val t2 = AVLNode(6, AVLNode(0), AVLNode(9))

    assert(intersect(t1)(t2) == AVLNil)
  }

  test("Intersection of two instances of one tree produces this tree"){
    val t1 = AVLNode(7, AVLNode(1), AVLNode(8))

    assert(intersect(t1)(t1) == t1)
  }

  test("Intersection of two trees is computed properly"){
    val t1 = AVLNode(6, AVLNode(0), AVLNode(8))
    val t2 = AVLNode(6, AVLNode(0), AVLNode(9))

    val temp = AVLNil
    val temp1 = insert(6)(temp)
    val temp2 = insert(0)(temp1)

    assert(intersect(t1)(t2) == temp2)
  }
}


