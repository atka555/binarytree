import scala.annotation.tailrec //@tailrec 
import scala.io.StdIn.{readLine, readInt}


trait BinaryTree { 

  // sumTree sum every value of leaf in the tree
  def sumTree(): Int = {
    def help(tree: BinaryTree, acc: Int): Int = tree match{
      case Leaf(value) => acc + value
      case Node(left, right) => help(left, acc) + help(right, acc)
    }
    help(this, 0)
  }

  // addLeaf adds leaf to balance the tree
  def addLeaf(leaf: Leaf): BinaryTree = this match{
    case Leaf(value) => Node(Leaf(value), leaf)
    case Node(left, right) => 
      if (help(this, 0) < 0) Node(left, right.addLeaf(leaf))
      else Node(left.addLeaf(leaf), right)
  }
  
  // multiplies all elements of the tree
  def multiplyElementsOfTree() : Int = {
      def recMult(tree: BinaryTree, acc: Int) : Int = tree match {
          case Leaf(value) => acc * value
          case Node(left, right) => recMult(left, acc) * recMult(right, acc)
      }
      recMult(this, 1)
  }
  
  def help(tree: BinaryTree, acc: Int): Int = tree match{
      case Leaf(_) => acc + 1
      case Node(left, right) => help(left, acc) - help(right, acc)
    }
    
  // creates new Tree, which consists of two trees
  def cloneTree() : BinaryTree = Node(this, this)
  
}

case class Leaf(value: Int) extends BinaryTree
case class Node(left: BinaryTree, right: BinaryTree) extends BinaryTree


object Main extends App {
  val tree = Node(Node(Leaf(1), Leaf(2)), Leaf(3))

  println(tree.sumTree())
  println(tree.addLeaf(Leaf(4)))

  println("multiplication testing!")
  val secTree = Node(Node(Leaf(1), Leaf(2)), Node(Leaf(3), Leaf(4)))
  println(secTree.multiplyElementsOfTree())
  val clonedTree = secTree.cloneTree()
  println(clonedTree.multiplyElementsOfTree())
  println(clonedTree)
}