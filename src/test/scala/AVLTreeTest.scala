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
}
