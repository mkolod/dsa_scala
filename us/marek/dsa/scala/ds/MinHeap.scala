package us.marek.dsa.scala.ds

import scala.collection.mutable.ArrayBuffer
import scala.annotation.tailrec

class MinHeap[T <% Ordered[T]: Manifest](initialSize: Int = 2) {

  private var currentSize = 0
  private var arr = new Array[T](initialSize)

  def parent(i: Int) = {
    val x = (i - 1) / 2
    if (x >= 0) Some(x) else None
  }

  def leftChild(i: Int) = {
    val x = 2 * i + 1
    if (x < currentSize) Some(x) else None
  }

  def rightChild(i: Int) = {
    val x = 2 * i + 2
    if (x < currentSize) Some(x) else None
  }

  def checkOverflow() = {
    if (currentSize + 1 == arr.length) {
      resize()
    }
  }

  def resize() = {
    val temp = new Array[T](math.max(1, arr.size) * 2)
    for (i <- 0 until currentSize) {
      temp(i) = arr(i)
    }
    arr = temp
  }

  def insert(t: T) = {
    checkOverflow()
    arr(currentSize) = t
    trickleUp()
    currentSize += 1
  }

  def trickleDown() = {
    var current = 0
    val temp = arr(current)

    while (current != -1) {
      var lc = leftChild(current)
      var rc = rightChild(current)
      val child = (lc, rc) match {
        case (Some(x), Some(y)) => Some(if (x > y) x else y)
        case (Some(x), None) => Some(x)
        case (None, Some(y)) => Some(y)
        case (None, None) => {println(s"lc=$lc, rc=$rc"); None}
      }
      if (child != None && arr(child.get) > arr(current)) {
        arr(current) = arr(child.get)
        current = child.get
      } else {
        arr(current) = temp
        current = -1
      }
    }
  }

  def trickleUp() = {
    println("trickleUp")
    var current = currentSize
    var par = parent(current)
    val temp = arr(current)
    while (par != None && arr(par.get) < arr(current)) {
      println("criterion")
      arr(current) = arr(par.get)
      current = par.get
      par = parent(par.get)
    }
    arr(current) = temp
    println(arr.toList)

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
  (1 to 10).foreach(mh.insert)
  println(mh.arr.toList)
//  while (!mh.isEmpty) {
//    println(mh.remove)
 // }

}

           