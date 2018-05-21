import AVLNode.getBalance

sealed abstract class AVLTree[+A]

case object AVLNil extends AVLTree[Nothing]

case class AVLNode[+A](data: A, left: AVLTree[A], right: AVLTree[A]) extends AVLTree[A] {
  def this(data: A) = this(data, AVLNil, AVLNil)
}

object AVLNode {
  def apply[A](data: A): AVLNode[A] = new AVLNode(data)

  def insert[A: Ordering](dataInsert: A)(tree: AVLTree[A]): AVLNode[A] = {
    val treeInserted = insertUnbalanced(dataInsert)(tree)
    val balance = getBalance(treeInserted)

    if (balance < -1) {
      treeInserted.right match {
        case AVLNil => throw new RuntimeException("Unknown error")
        case AVLNode(data: A, _, _) =>
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
        case AVLNode(data: A, _, _) =>
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

  def insertUnbalanced[A: Ordering](dataInsert: A)(tree: AVLTree[A]): AVLNode[A] = {
    tree match {
      case AVLNil => AVLNode(dataInsert)
      case t: AVLNode[A] =>
        val order = Ordering[A].compare(dataInsert, t.data)
        if (order > 0) {
          t.right match {
            case AVLNil => AVLNode(t.data, t.left, AVLNode(dataInsert))
            case x: AVLNode[A] => AVLNode(t.data, t.left, insert(dataInsert)(x))
          }
        } else if (order < 0) {
          t.left match {
            case AVLNil => AVLNode(t.data, AVLNode(dataInsert), t.right)
            case x: AVLNode[A] => AVLNode(t.data, insert(dataInsert)(x), t.right)
          }
        } else // value already in a tree
          t

    }
  }

  def height[A](tree: AVLTree[A]): Int = {
    tree match {
      case AVLNil => 0
      case AVLNode(_, left, right) => 1 + math.max(height(left), height(right))
    }
  }

  def search[A: Ordering](value: A)(tree: AVLTree[A]): Boolean = {
    tree match {
      case AVLNil => false
      case AVLNode(data, left, right) =>
        val ord = Ordering[A].compare(value, data)
        ord match {
          case 0 => true
          case 1 => search(value)(right)
          case _ => search(value)(left)
        }
    }
  }

  def getBalance[A](tree: AVLTree[A]): Int = {
    tree match {
      case AVLNil => 0
      case AVLNode(_, left, right) => height(left) - height(right)
    }
  }

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

  def remove[A: Ordering](dataRemove: A)(tree: AVLTree[A]): AVLTree[A] = {
    val deletedImbalanced = removeUmbalanced(dataRemove)(tree)
    val balance = getBalance(tree)

    deletedImbalanced match {
      case AVLNil => AVLNil
      case AVLNode(data, left, right) =>
        if (balance > 1) {
          if (getBalance(left) >= 0){ // left left case
            rightRotate(deletedImbalanced)
          } else { // left right case
            AVLNode(data, leftRotate(deletedImbalanced), right)
          }
        } else if (balance < -1){
          if (getBalance(right) <= 0){ // right right case
            leftRotate(deletedImbalanced)
          } else { // right left case
            AVLNode(data, left, rightRotate(deletedImbalanced))
          }
        } else {
          deletedImbalanced
        }
    }

  }

  def removeUmbalanced[A: Ordering](dataRemove: A)(tree: AVLTree[A]): AVLTree[A] = {
    tree match {
      case AVLNil => tree // exception??
      case AVLNode(data, left, right) =>
        val order = Ordering[A].compare(dataRemove, data)
        order match {
          case -1 => AVLNode(data, removeUmbalanced(dataRemove)(left), right)
          case 1 => AVLNode(data, left, removeUmbalanced(dataRemove)(right))
          case 0 =>
            if (left == AVLNil || right == AVLNil){ // no child or one child
              val temp = if (left != AVLNil) left else right
              temp match {
                case AVLNil => AVLNil // no child case
                case AVLNode(dataChild, leftChild, rightChild) => AVLNode(dataChild, leftChild, rightChild) // one child case
              }
            }
            else { // two children case, get the inorder successor (smallest key value)
              val successorValue = getMinValue(right)
              AVLNode(successorValue, left, removeUmbalanced(successorValue)(right))
            }
        }
    }
  }


}