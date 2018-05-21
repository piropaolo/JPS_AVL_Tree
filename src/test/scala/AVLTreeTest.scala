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
    assert(AVLNode.search(2)(t1))
  }

  test("Value found if is present in multiple nodes tree"){
    val t1:AVLNode[Int] = AVLNode(1, AVLNode(0, AVLNode(-1), AVLNode(2)), AVLNode(3, AVLNode(4), AVLNode(5, AVLNode(6), AVLNode(7))))
    assert(AVLNode.search(7)(t1))
  }

  test("Value not found if not present"){
    val t1 = AVLNode(2)
    assert(!AVLNode.search(3)(t1))
  }

  test("Empty tree has height equal to 0"){
    val t1 = AVLNil
    assert(AVLNode.height(t1) == 0)
  }

  test("One-node tree has height equal to 1"){
    val t1 = AVLNode(1)
    assert(AVLNode.height(t1) == 1)
  }

  test("Multiple nodes tree has proper height"){
    val t1 = AVLNode(1, AVLNode(0, AVLNode(-1, AVLNil)), AVLNode(3, AVLNil, AVLNode(5, AVLNil, AVLNode(7))))
    assert(AVLNode.height(t1) == 4)
  }

  test("Tree has proper height after insertion"){
    val t1 = AVLNode(3)
    val t2 = AVLNode.insert(4)(t1)
    assert(AVLNode.height(t2) == 2)
  }

  test("Value present after insertion"){
    val t1 = AVLNode(3)
    val t2 = AVLNode.insert(4)(t1)
    assert(AVLNode.search(4)(t2))
  }

  test("Empty tree is balanced"){
    val t1 = AVLNil
    assert(AVLNode.getBalance(t1) == 0)
  }

  test("One-node tree is balanced"){
    val t1 = AVLNode(2)
    assert(AVLNode.getBalance(t1) == 0)
  }

  test("Node with left children is unbalanced"){
    val t1 = AVLNode(2, AVLNode(1, AVLNode(3), AVLNil), AVLNil)
    assert(AVLNode.getBalance(t1) > 1)
  }

  test("Node with right children is unbalanced"){
    val t1 = AVLNode(2, AVLNil, AVLNode(3, AVLNode(1), AVLNil))
    assert(AVLNode.getBalance(t1) < -1)
  }

  test("Exception is thrown after right rotation on too small tree"){
    val t1 = AVLNode(10)
    assertThrows[RuntimeException](AVLNode.rightRotate(t1))
  }

  test("Exception is thrown after left rotation on too small tree"){
    val t1 = AVLNode(10)
    assertThrows[RuntimeException](AVLNode.leftRotate(t1))
  }

  test("Right rotation performed correctly"){
    val tree = AVLNode(10, AVLNode(7, AVLNode(6), AVLNode(8)), AVLNode(12))

    val rotated = AVLNode.rightRotate(tree)
    val expected = AVLNode(7, AVLNode(6), AVLNode(10, AVLNode(8), AVLNode(12)))

    assert(rotated == expected)
  }

  test("Left rotation performed correctly"){
    val tree = AVLNode(7, AVLNode(6), AVLNode(10, AVLNode(8), AVLNode(12)))

    val rotated = AVLNode.leftRotate(tree)
    val expected = AVLNode(10, AVLNode(7, AVLNode(6), AVLNode(8)), AVLNode(12))

    assert(rotated == expected)
  }

  test("Tree after right rotation is balanced"){
    val tree = AVLNode(5, AVLNode(3, AVLNode(2), AVLNil), AVLNode(7))

    val rotated = AVLNode.rightRotate(tree)

    assert(AVLNode.getBalance(rotated) == -1)
  }

  test("Tree after left rotation is balanced"){
    val tree = AVLNode(5, AVLNode(3), AVLNode(3, AVLNil, AVLNode(4)))

    val rotated = AVLNode.leftRotate(tree)

    assert(AVLNode.getBalance(rotated) == 1)
  }

  test("Tree after several inserts is balanced"){
    val t1 = AVLNode(20)

    val t2 = AVLNode.insert(10)(t1)
    val t3 = AVLNode.insert(5)(t2)
    val t4 = AVLNode.insert(3)(t3)
    val t5 = AVLNode.insert(1)(t4)
    val t6 = AVLNode.insert(22)(t5)
    val t7 = AVLNode.insert(25)(t6)
    val t8 = AVLNode.insert(30)(t7)

    assert(AVLNode.getBalance(t8) == -1)
  }

  test("Maximum value is returned"){
    val tree = AVLNode(5, AVLNode(3), AVLNode(6))
    val tree2 = AVLNode.insert(10)(tree)

    val maxValue = AVLNode.getMaxValue(tree2)

    assert(maxValue == 10)
  }

  test("Minimum value is returned"){
    val tree = AVLNode(5, AVLNode(3), AVLNode(6, AVLNil, AVLNode(7)))
    val tree2 = AVLNode.insert(12)(tree)

    val minValue = AVLNode.getMinValue(tree2)

    assert(minValue == 3)
  }


  test("Node with one child is deleted"){
    val tree = AVLNode(5, AVLNode(3), AVLNode(6, AVLNil, AVLNode(7)))

    val treeDeleted = AVLNode.remove(6)(tree)

    assert(!AVLNode.search(6)(treeDeleted))
  }

  test("Root node is deleted"){
    val tree = AVLNode(5, AVLNode(3), AVLNode(6, AVLNil, AVLNode(7)))

    val treeDeleted = AVLNode.remove(5)(tree)

    assert(!AVLNode.search(5)(treeDeleted))
  }


  test("Node with no children is deleted"){
    val tree = AVLNode(5, AVLNode(3), AVLNode(6, AVLNil, AVLNode(7)))

    val treeDeleted = AVLNode.remove(7)(tree)

    assert(!AVLNode.search(7)(treeDeleted))
  }


  test("Tree is balanced after deletion of node with one child"){
    val tree = AVLNode(5, AVLNode(3), AVLNode(6, AVLNil, AVLNode(7)))

    val treeDeleted = AVLNode.remove(6)(tree)

    assert(math.abs(AVLNode.getBalance(treeDeleted)) <= 1)
  }

  test("Tree is balanced after deletion of root node"){
    val tree = AVLNode(5, AVLNode(3), AVLNode(6, AVLNil, AVLNode(7)))

    val treeDeleted = AVLNode.remove(5)(tree)

    assert(math.abs(AVLNode.getBalance(treeDeleted)) <= 1)
  }


  test("Tree is balanced after deletion of node with no children"){
    val tree = AVLNode(5, AVLNode(3), AVLNode(6, AVLNil, AVLNode(7)))

    val treeDeleted = AVLNode.remove(7)(tree)

    assert(math.abs(AVLNode.getBalance(treeDeleted)) <= 1)
  }

  test("Tree is the same after deletion value not present in original tree"){
    val tree = AVLNode(5, AVLNode(3), AVLNode(6, AVLNil, AVLNode(7)))

    val treeDeleted = AVLNode.remove(70)(tree)

    assert(tree == treeDeleted)
  }



}


