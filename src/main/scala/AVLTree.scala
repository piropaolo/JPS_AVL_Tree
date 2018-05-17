sealed abstract class AVLTree[+A]

case object AVLNil extends AVLTree[Nothing]

case class AVLNode[+A] (data: A, left: AVLTree[A], right: AVLTree[A]) extends AVLTree[A]{
  def this(data: A) = this(data, AVLNil, AVLNil)
}

object AVLNode{
  def apply[A](data: A): AVLNode[A] = new AVLNode(data)
}