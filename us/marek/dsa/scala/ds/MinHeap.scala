package us.marek.dsa.scala.ds

import scala.collection.mutable.ArrayBuffer
import scala.annotation.tailrec

class MinHeap[T <% Ordered[T]: Manifest](initialSize: Int = 2) {

  private var currentSize = 0
  private var arr = new Array[T](initialSize)

  private def parent(i: Int) = {
    val x = (i - 1) / 2
    if (x >= 0) Some(x) else None
  }

  private def leftChild(i: Int) = {
    val x = 2 * i + 1
    if (x < currentSize) Some(x) else None
  }

  private def rightChild(i: Int) = {
    val x = 2 * i + 2
    if (x < currentSize) Some(x) else None
  }

  private def checkCapacity() = {
    if (currentSize == arr.length) {
      resize()
    }
  }

  private def resize() = {
    val temp = new Array[T](math.max(1, arr.size) * 2)
    for (i <- 0 until currentSize) {
      temp(i) = arr(i)
    }
    arr = temp
  }

  def insert(t: T) = {
    checkCapacity()
    arr(currentSize) = t
    trickleUp()
    currentSize += 1
  }

  private def trickleDown() = {
    var current = 0
    val temp = arr(current)

    while (current != -1) {
      var lc = leftChild(current)
      var rc = rightChild(current)
      val child = (lc, rc) match {
        case (Some(x), Some(y)) => Some(if (arr(x) < arr(y)) lc.get else rc.get)
        case (Some(x), None) => Some(lc.get)
        case (None, Some(y)) => Some(rc.get)
        case (None, None) => None
      }
      if (child != None && arr(child.get) < arr(current)) {
        swap(current, child.get)
        current = child.get
      } else {
        arr(current) = temp
        current = -1
      }
    }
  }
  
  private def swap(i: Int, j: Int) = {
    val temp = arr(i)
    arr(i) = arr(j)
    arr(j) = temp
  }

  private def trickleUp() = {
    var current = currentSize
    var par = parent(current)
    while (par != None && arr(par.get) > arr(current)) {
      println(s"In while, par=$par, arr(par.get)=${arr(par.get)}, arr(current)=${arr(current)}")
      swap(par.get, current)
      current = par.get
      par = parent(current)
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
  val foo = Array.fill(10)(scala.util.Random.nextInt(1000)).toSet.toList
  println(foo)
  foo.foreach(mh.insert)
  while (!mh.isEmpty) {
    println(mh.remove)
  }

}

           