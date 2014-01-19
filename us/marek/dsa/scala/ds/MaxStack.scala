package us.marek.dsa.scala.ds 

/* need to import because while StackOverflowError is in java.lang,
   EmptyStackException is in java.util */
import java.util.EmptyStackException

/**
 * Stack with O(1) push(), pop() and getMax()
 * 
 * @param maxSize - maximum size of the stack
 */
class MaxStack[T <% Ordered[T]](val maxSize: Int) {
  
  /* wrapper class to keep track of max for each item pushed
     onto the stack - can't keep a single max value for the 
     whole stack because while such a max would be correct 
     while pushing, it wouldn't be an O(1) operation while
     popping - if we popped the item that was the max so 
     far, we wouldn't know where the previos max was; 
     so, we'll have to keep track of it for each element
   */
  private[MaxStack] class Wrapper[T](val t: T, val max: T)

  // this array will hold the stack's data
  private val arr = new Array[Wrapper[T]](maxSize)
  
  /* points to top of the stack; if stack is empty, use index -1
     since index 0 would already contain data
   */
  private var position = -1

  def push(t: T): Unit = {
    if (isFull) throw new StackOverflowError("Stack overflow") 
    position += 1
    // if no data in stack, use current element, otherwise check last pushed element
    val currentMax = if (position == 0) t else arr(position - 1).max    
    // update max if need be
    val newMax = if (t > currentMax) t else currentMax
    arr(position) = new Wrapper(t, newMax)  
  }
  
  // push many items at a time 
  def push(varargs: T*): Unit = varargs.foreach(push)

  def pop: T  = {
    /* instead of throwing an exception, we could return None,
    in which case the return type would be Option[T] */
    val r = top
    position -= 1
    r.t
  }
  
  def getMax: T = top.max
   
  def size: Int = position + 1

  def peek: T = top.t
 
  private def top = position match {
    case -1 => throw new EmptyStackException()
    case _ => arr(position)
  }
  
  def isEmpty: Boolean = position == -1
  def isFull: Boolean = position == maxSize - 1
  override def toString = s"MaxStack(${arr.map(_.t).mkString(",")}) (top on the right)"
  
}


object MaxStack extends App {
  
  val stack = new MaxStack[Int](10)
  
  println("\nMaxStack test\n")
  
  for (i <- Seq(3, 2, 1, 6, 7, 9, 5, 8, 4, 0)) {
    stack.push(i)
    println(s"Pushed $i, top of the stack =  ${stack.peek}, current max = ${stack.getMax}, current size = ${stack.size}")
  }
  
  println(s"\n$stack\n")
  
  for (i <- 1 to 10) {
    val popped = stack.pop
    print(s"Popped $popped")
    if (!stack.isEmpty) {
      println(s", top of stack = ${stack.peek}, current max = ${stack.getMax}, current size = ${stack.size}")
    } else {
      println(s", stack is empty, current size = ${stack.size}")
    }
  }
  
}