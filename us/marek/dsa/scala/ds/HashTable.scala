package us.marek.dsa.scala.ds

import scala.collection.mutable.ArrayBuffer
import scala.util.Random

// try writing my own arraybuffer, or better yet, use a linked list

object HashTable extends App {
  
  val ht = HashTable[Int](10)
  val rand = new Random()
  for (i <- 1 to 10000) ht.add(rand.nextInt)
  ht.collisions
}

case class HashTable[T](hashSize: Int) {
  
  val arr = Array.fill(hashSize)(new ArrayBuffer[T]())
  
  def findArr[T](t: T) = arr(math.abs(t.hashCode % hashSize))
  
  def add(t: T) = {
    val ll = findArr(t)
    if (find(t)) false else {
      ll += t
      true
    }
  }
  
  def find[T](t: T) = {
    val ll = findArr(t)
    ll.find(x => x == t) != None  
  }
  
  def remove(t: T) = {
    val ll = findArr(t)
    val llElem = ll.indexOf(t)
    ll.remove(llElem)
  }
  
  def numCollisions[S >: T](s: S) = findArr(s).size - 1
  def collisions = for (i <- 0 until arr.size) println("Array " + i + " has " + (arr(i).size - 1) + " collisions")
  
  override def toString = (for (i <- arr; j <- i) yield j).toList.mkString(",")
  
}