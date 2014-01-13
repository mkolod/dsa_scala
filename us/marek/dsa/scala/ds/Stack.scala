package us.marek.dsa.scala.ds

class Stack[T : Manifest](maxSize: Int) {
  
  private[Stack] class StackUnderflowError(s: String) extends Error(s)
  
  val arr = new Array[T](maxSize)
  var currentSize = 0
  
  def push[S <: T](s: S) = {
    if (isFull) throw new StackOverflowError(s"Stack overflow - stack size = $maxSize")
    arr(currentSize) = s
    currentSize += 1
  }
  
  def pop[S <: T] = {
    if (isEmpty) throw new StackUnderflowError("Stack underflow")
    currentSize -= 1
    arr(currentSize)
  }
  
  def peek = if (isEmpty) throw new StackUnderflowError("Stack underflow") else arr(currentSize - 1)
  
  def isEmpty = currentSize == 0
  def isFull = currentSize == maxSize  
  def size = currentSize

}

object Stack extends App {
  
  val foo = new Stack[Int](5)
  
  for (i <- 1 to 5) foo.push(i)
  
  println(s"top of stack = ${foo.peek}")
  
  for (i <- 1 to 5) foo.pop
  
  println(s"current size = ${foo.size}")
  
}