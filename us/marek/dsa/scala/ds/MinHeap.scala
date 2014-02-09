package us.marek.dsa.scala.ds

import scala.collection.mutable.ArrayBuffer
import scala.annotation.tailrec
import scala.util.Random

class MinHeap[T <% Ordered[T]: Manifest](initialSize: Int = 2) {

  private var currentSize = 0
  private var arr = new Array[T](initialSize)

  private def parent(i: Int): Option[Int] = {
    val x = (i - 1) / 2
    if (x >= 0) Some(x) else None
  }

  private def leftChild(i: Int): Option[Int] = {
    val x = 2 * i + 1
    if (x < currentSize) Some(x) else None
  }

  private def rightChild(i: Int): Option[Int] = {
    val x = 2 * i + 2
    if (x < currentSize) Some(x) else None
  }

  private def checkCapacity(): Unit = {
    if (currentSize == arr.length) {
      resize()
    }
  }

  private def resize(): Unit = {
    val temp = new Array[T](math.max(1, arr.size) * 2)
    for (i <- 0 until currentSize) {
      temp(i) = arr(i)
    }
    arr = temp
  }

  def insert(t: T): Unit = {
    checkCapacity()
    arr(currentSize) = t
    trickleUp()
    currentSize += 1
  }

  @tailrec
  private def trickleDown(current: Int = 0): Unit = {
    val child = getSmallestChild(current)
    if (child != None && arr(child.get) < arr(current)) {
      swap(current, child.get)
      trickleDown(child.get)
    }
  }
  
  private def getSmallestChild(idx: Int): Option[Int] = {
    var lc = leftChild(idx)
    var rc = rightChild(idx)
    (lc, rc) match {
      case (Some(x), Some(y)) => Some(if (arr(x) < arr(y)) lc.get else rc.get)
      case (Some(x), None) => Some(lc.get)
      case (None, Some(y)) => Some(rc.get)
      case (None, None) => None
    }
  }
  
  private def swap(i: Int, j: Int): Unit = {
    val temp = arr(i)
    arr(i) = arr(j)
    arr(j) = temp
  }

  @tailrec
  private def trickleUp(current: Int = currentSize): Unit = {
    val par = parent(current)
    if (par != None && arr(par.get) > arr(current)) {
      swap(par.get, current)
      trickleUp(par.get)
    }
  }

  def remove(): T = {
    val temp = arr(0)
    arr(0) = arr(currentSize - 1)
    currentSize -= 1
    trickleDown()
    temp
  }

  def isEmpty = currentSize == 0

}

object MinHeap extends App {
  val mh = new MinHeap[Int]()
  val rand = new Random(1)
  val foo = Array.fill(10)(rand.nextInt(1000)).toSet.toList
  println(foo)
  foo.foreach(mh.insert)
  
  val buffer = ArrayBuffer[Int]()
  while (!mh.isEmpty) {
    buffer += mh.remove()
  }
  println(buffer.toList)

}

           