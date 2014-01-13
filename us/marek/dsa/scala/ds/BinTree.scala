package us.marek.dsa.scala.ds

import scala.annotation.tailrec

sealed case class Node[T : Manifest](val value: T, var leftChild: Option[Node[T]] = None, var rightChild: Option[Node[T]] = None) {
  
  override def toString = value.toString
  
  def inOrder[R](node: Option[Node[T]] = Some(this), f: T => R, default: T => R): R = node match {
    case Some(Node(value, left, right)) => {
      inOrder(left, f, default)
      f(value)
      inOrder(right, f, default)
    }
    case None => default(value)
  }
  
  def preOrder[R](node: Option[Node[T]] = Some(this), f: T => R, default: T => R): R = node match {
    case Some(Node(value, left, right)) => {
      f(value)
      preOrder(left, f, default) 
      preOrder(right, f, default)
    }
    case None => default(value)
  }
  
  def postOrder[R](node: Option[Node[T]] =  Some(this), f: T => R, default: T => R): R = node match {
    case Some(Node(value, left, right)) => {
      postOrder(left, f, default)
      postOrder(right, f, default)
      f(value)
    }
    case None => default(value)
  }
 
}


class BinTree[T <% Ordered[T] : Manifest](var root: Option[T] = None) {
  
  def this(t: T) = this(Some(t))
  
  val rootNode = root match {
    case None => None
    case x: Some[T] => Some(Node(x.get))
  }
  
  def insert(t: T): Unit = {
    
  }
  
  def find(t: T): Option[Node[T]] = find(t)
  def find(t: T, mut: Boolean): Option[Node[T]] = find(t, mutate = mut)
 // def find(t: T): Option[Node[T]] = find(t, mut = false)
    
  private def find(t: T, getParent: Boolean = false, mutate: Boolean = false): Option[Node[T]] = {
     
    def checkChildren(node: Option[Node[T]], modify: Boolean): Boolean = node match {
      case Some(Node(_, left: Some[Node[T]], _)) if left.get.value == t   => true 
      case Some(Node(_, _, right: Some[Node[T]])) if right.get.value == t => true
      case Some(Node(_, None, None)) if modify => true // this is for searching the parent for insertion
      case _ => false
    }
    
    @tailrec
    def recFind(node: Option[Node[T]]): Option[Node[T]] = {
      if (mutate && checkChildren(node, modify = true)) node
      else if (getParent && checkChildren(node, modify = false)) node else node match {
        case x: Some[Node[T]] if x.get.value == t => x
        case Some(elem: Node[T]) if (elem.value > t) => recFind(elem.leftChild)
        case Some(elem: Node[T]) if (elem.value < t) => recFind(elem.rightChild) 
        case None => None
        case _ => throw new IllegalStateException()
      } 
    }
    recFind(rootNode)
  }
  
  def contains(t: T): Boolean = find(t) != None
  
}


object BinTree extends App {
  
  val foo = new BinTree[Int](10)
 // for (i <- List(10)) foo.insert(i)
  
  // fix the insert method
  // add delete method
  // add rebalancing methods
  // calculate height
 
  val foo2 = foo.rootNode.get
  foo2.leftChild = Some(Node(5))
  foo2.rightChild = Some(Node(15))
  val foo3 = foo2.leftChild.get
  foo3.leftChild = Some(Node(1))
  foo3.rightChild = Some(Node(6))
  val foo4 = foo2.rightChild.get
  foo4.leftChild = Some(Node(11))
  foo4.rightChild = Some(Node(21))

 
 foo.rootNode.get.preOrder(f = (x: Int) => print(x + " "), default = (x: Int) => ())
 // x => print(x + " "), default = x => ()
 
 // x => print(x + " ")
}