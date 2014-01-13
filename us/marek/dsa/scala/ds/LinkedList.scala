package us.marek.dsa.scala.ds

import scala.collection.mutable.StringBuilder

abstract class LLNode[+T](val elem: Option[T], val next: LLNode[T])
case class ValNode[T](override val elem: Option[T], override val next: LLNode[T] = NilNode) extends LLNode[T](elem, next)
case object NilNode extends LLNode[Nothing](None, null) { 
  override def toString = ""
}

case class LinkedList[T](val first: LLNode[T]) {
  
 // def this() = this(NilNode)
  def this(t: T) = this(ValNode(Some(t)))
  
  def head = first match {
    case NilNode => NilNode
    case ValNode(elem, next) => val y = ValNode(elem, NilNode) 
  }
  
  def tail = first match {
    case ValNode(h, t) => t
    case NilNode => NilNode
  }
  
  
  override def toString = {
    var s = first.toString  
    tail match {
      case NilNode => s
      case ValNode(e, n) => s + "," + e + n.toString
    }
    
  } 
    

}

object LinkedList {
  
  /*
  implicit def wrap[T](elem: T): Option[T] = Some(elem)
  
  def apply() = new LinkedList()
  def apply[T](elem: T) = new LinkedList[T](elem)
  def apply[T](elem: T, next: T) = new LinkedList[T](elem, next)
  def empty[T] = new LinkedList[T]()
  
  object Empty extends LinkedList[Nothing]
  * 
  */
  
}


object Foo extends App {
  val x = new LinkedList(1, new ValNode(Some(2)))
  println(x.head)
  println(x.first.elem)
}