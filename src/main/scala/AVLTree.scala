import scala.annotation.tailrec

sealed abstract class AVLTree[+A]

/**Null-Node object.
  *
  */
case object AVLNil extends AVLTree[Nothing]

/**AVL tree main structure. Tree objects are immutable.
  *
  * @param data data stored in a node
  * @param left left child of a node
  * @param right right child of a node
  * @tparam A data type
  */
case class AVLNode[+A](data: A, left: AVLTree[A], right: AVLTree[A]) extends AVLTree[A] {
  def this(data: A) = this(data, AVLNil, AVLNil)
}


/** Main object providing methods to operate on trees
  *
  */
object AVLNode {
  /**Constructor for Node without children
    *
    * @param data data stored in that Node
    * @tparam A data type
    * @return new Node
    */
  def apply[A](data: A): AVLNode[A] = new AVLNode(data)


  /**Inserts given value into a given tree. Binary Search Tree properties are preserved,
    * tree is balanced after operation. Balancing is done via rotations.
    *
    * @param dataInsert data to insert into tree
    * @param tree tree to perform operation on
    * @tparam A data type
    * @return new, balanced tree with inserted value
    */
  def insert[A: Ordering](dataInsert: A)(tree: AVLTree[A]): AVLNode[A] = {
    val treeInserted = insertUnbalanced(dataInsert)(tree)
    val balance = getBalance(treeInserted)

    if (balance < -1) {
      treeInserted.right match {
        case AVLNil => throw new RuntimeException("Unknown error")
        case AVLNode(data, _, _) =>
          val order = Ordering[A].compare(dataInsert, data)
          order match {
            case 1 => leftRotate(treeInserted) // right right case
            case -1 => // right left case
              AVLNode(treeInserted.data, treeInserted.left, rightRotate(treeInserted.right))
              leftRotate(tree)
          }
      }
    } else if (balance > 1) {
      treeInserted.left match {
        case AVLNil => throw new RuntimeException("Unknown error")
        case AVLNode(data, _, _) =>
          val order = Ordering[A].compare(dataInsert, data)
          order match {
            case 1 => // left right case
              val tree = AVLNode(treeInserted.data, leftRotate(treeInserted.left), treeInserted.right)
              rightRotate(tree)
            case -1 => rightRotate(treeInserted) // left left case
          }
      }
    } else {
      treeInserted
    }
  }

  /**Helper method that inserts value into a given tree, but tree after operation is unbalanced (BST properties
    * are preserved)
    *
    * @param dataInsert data to insert into tree
    * @param tree tree to perform operation on
    * @tparam A data type
    * @return unbalanced tree with inserted value
    */
  def insertUnbalanced[A: Ordering](dataInsert: A)(tree: AVLTree[A]): AVLNode[A] = {
    tree match {
      case AVLNil => AVLNode(dataInsert)
      case t: AVLNode[A] =>
        val order = Ordering[A].compare(dataInsert, t.data)
        if (order > 0) { // value needs to be inserted in a right sub-tree
          t.right match {
            case AVLNil => AVLNode(t.data, t.left, AVLNode(dataInsert)) // right subtree is empty
            case x: AVLNode[A] => AVLNode(t.data, t.left, insert(dataInsert)(x)) // return tree with replaced right sub-tree
          }
        } else if (order < 0) {
          t.left match { // value needs to be inserted in a left sub-tree
            case AVLNil => AVLNode(t.data, AVLNode(dataInsert), t.right) // left subtree is empty
            case x: AVLNode[A] => AVLNode(t.data, insert(dataInsert)(x), t.right) // return tree with replaced left sub-tree
          }
        } else // value already in a tree
          t

    }
  }

  /**Return height of a tree, if tree is AVLNil, 0 is returned
    *
    * @param tree tree to measure height on
    * @tparam A type of a data of a tree
    * @return height of a tree
    */
  def height[A](tree: AVLTree[A]): Int = {
    tree match {
      case AVLNil => 0
      case AVLNode(_, left, right) => 1 + math.max(height(left), height(right))
    }
  }

  /**Searches for given value in a given tree. Returns tree if search was successful,
    * false if unsuccessful or tree was empty
    *
    * @param value value to be searched in a tree
    * @param tree tree to perform operation on
    * @tparam A data type
    * @return true if value present in a tree, false if not
    */
  def search[A: Ordering](value: A)(tree: AVLTree[A]): Boolean = {
    tree match {
      case AVLNil => false
      case AVLNode(data, left, right) =>
        val ord = Ordering[A].compare(value, data)
        ord match {
          case 0 => true // value was found
          case 1 => search(value)(right) // value is in the right sub-tree
          case _ => search(value)(left) // value is in the left sub-tree
        }
    }
  }

  /**Method measures balance of a tree by measuring difference between height of a left child
    *  and height of a right child. Tree is balanced if -1, 0 or 1 is returned.
    *
    * @param tree tree to perform operation on
    * @tparam A data type
    * @return returns height difference between left child and right child node
    */
  def getBalance[A](tree: AVLTree[A]): Int = {
    tree match {
      case AVLNil => 0
      case AVLNode(_, left, right) => height(left) - height(right)
    }
  }

  /**Performs right rotation on a given tree. If rotations cannot be performed (too few Nodes), exception is thrown.
    *
    *           y                               x
    *          / \     Right Rotation          /  \
    *        x   T3   – – – – – – – >        T1   y
    *       / \       < - - - - - - -            / \
    *      T1  T2     Left Rotation            T2  T3
    *
    * @param tree tree to perform rotation on
    * @tparam A data type
    * @return rotated tree
    */
  def rightRotate[A](tree: AVLTree[A]): AVLNode[A] = {
    tree match {
      case AVLNil => throw new RuntimeException("Cannot perform rotation on that node")
      case AVLNode(data, left, right) =>
        left match {
          case AVLNil => throw new RuntimeException("Cannot perform rotation on that node")
          case AVLNode(leftData, leftLeft, leftRight) => AVLNode(leftData, leftLeft, AVLNode(data, leftRight, right))
        }
    }
  }

  /**Performs left rotation on a given tree. If rotations cannot be performed (too few Nodes), exception is thrown.
    *
    *           y                               x
    *          / \     Right Rotation          /  \
    *        x   T3   – – – – – – – >        T1   y
    *       / \       < - - - - - - -            / \
    *      T1  T2     Left Rotation            T2  T3
    *
    * @param tree tree to perform rotation on
    * @tparam A data type
    * @return rotated tree
    */
  def leftRotate[A](tree: AVLTree[A]): AVLNode[A] = {
    tree match {
      case AVLNil => throw new RuntimeException("Cannot perform rotation on that node")
      case AVLNode(data, left, right) =>
        right match {
          case AVLNil => throw new RuntimeException("Cannot perform rotation on that node")
          case AVLNode(rightData, rightLeft, rightRight) => AVLNode(rightData, AVLNode(data, left, rightLeft), rightRight)
        }
    }
  }

  /**Returns minimum value of a tree (value of the leftmost node). If tree is empty, exception is thrown.
    *
    * @param tree tree to perform operation on
    * @tparam A data type
    * @return minimum value of a tree
    */
  def getMinValue[A](tree: AVLTree[A]): A = {
    tree match {
      case AVLNil => throw new RuntimeException("Empty tree")
      case AVLNode(data, left, _) =>
        left match {
          case AVLNil => data
          case x: AVLNode[A] => getMinValue (x)
        }
    }
  }

  /**Returns maximum value of a tree (value of the rightmost node). If tree is empty, exception is thrown.
    *
    * @param tree tree to perform operation on
    * @tparam A data type
    * @return maximum value of a tree
    */
  def getMaxValue[A](tree: AVLTree[A]): A = {
    tree match {
      case AVLNil => throw new RuntimeException("Empty tree")
      case AVLNode(data, _, right) =>
        right match {
          case AVLNil => data
          case x: AVLNode[A] => getMaxValue (x)
        }
    }
  }


  /**Removes given value from a given tree. Binary Search Tree properties are preserved,
    * tree is balanced after operation. Balancing is done via rotations.
    *
    * @param dataRemove data to remove from a tree
    * @param tree tree to perform operation on
    * @tparam A data type
    * @return new, balanced tree with removed value
    */
  def remove[A: Ordering](dataRemove: A)(tree: AVLTree[A]): AVLTree[A] = {
    val deletedUnbalanced = removeUnbalanced(dataRemove)(tree)
    val balance = getBalance(tree)

    deletedUnbalanced match {
      case AVLNil => AVLNil // tree is empty
      case AVLNode(data, left, right) =>
        if (balance > 1) {
          if (getBalance(left) >= 0){ // left left case
            rightRotate(deletedUnbalanced)
          } else { // left right case
            AVLNode(data, leftRotate(deletedUnbalanced), right)
          }
        } else if (balance < -1){
          if (getBalance(right) <= 0){ // right right case
            leftRotate(deletedUnbalanced)
          } else { // right left case
            AVLNode(data, left, rightRotate(deletedUnbalanced))
          }
        } else {
          deletedUnbalanced // tree is balanced
        }
    }

  }

  /**Helper method that removes given value from a given tree, but tree after operation is unbalanced (BST properties
    * are preserved)
    *
    * @param dataRemove data to insert into tree
    * @param tree tree to perform operation on
    * @tparam A data type
    * @return unbalanced tree with removed value
    */
  def removeUnbalanced[A: Ordering](dataRemove: A)(tree: AVLTree[A]): AVLTree[A] = {
    tree match {
      case AVLNil => tree // tree is empty
      case AVLNode(data, left, right) =>
        val order = Ordering[A].compare(dataRemove, data)
        order match {
          case -1 => AVLNode(data, removeUnbalanced(dataRemove)(left), right) // value to remove is in the left sub-tree
          case 1 => AVLNode(data, left, removeUnbalanced(dataRemove)(right)) // value to remove is in the right sub-tree
          case 0 => // value to remove is in current node
            if (left == AVLNil || right == AVLNil){ // no child or one child
              val temp = if (left != AVLNil) left else right
              temp match {
                case AVLNil => AVLNil // no child case
                case AVLNode(dataChild, leftChild, rightChild) => AVLNode(dataChild, leftChild, rightChild) // one child case
              }
            }
            else { // two children case, get the inorder successor (smallest key value)
              val successorValue = getMinValue(right)
              AVLNode(successorValue, left, removeUnbalanced(successorValue)(right))
            }
        }
    }
  }

  @tailrec
  def union[A: Ordering](that: AVLTree[A])(tree: AVLTree[A]): AVLTree[A] =
    that match {
      case AVLNil => tree
      case AVLNode(data, _, _) => union(remove(data)(that))(insert(data)(tree))
    }


  def intersect[A: Ordering](that: AVLTree[A])(tree: AVLTree[A]): AVLTree[A] = {
    @tailrec
    def innerIntersect(that: AVLTree[A])(tree: AVLTree[A])(res: AVLTree[A] = AVLNil): AVLTree[A] = {
      that match {
        case AVLNil => res
        case AVLNode(data, _, _) =>
          if (search(data)(tree))
            innerIntersect(remove(data)(that))(tree)(insert(data)(res))
          else
            innerIntersect(remove(data)(that))(tree)(res)
      }
    }
    innerIntersect(that)(tree)()
  }
}