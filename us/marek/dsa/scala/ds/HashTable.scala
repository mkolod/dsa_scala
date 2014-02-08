package us.marek.dsa.scala.ds

import scala.collection.mutable.ArrayBuffer
import scala.util.Random
import HashUtils.{md5, sha1, sha256, murmurHash3}
import HashType._

object HashTable extends App {
  
  val ht = HashTable[Int](10, MurmurHash3)
  val rand = new Random(1)
  for (i <- 1 to 10000) ht.add(rand.nextInt)
  ht.collisions
}

case class HashTable[T](hashSize: Int, hashType: HashType = HashCode) {
  
  val arr = Array.fill(hashSize)(new ArrayBuffer[T]())
  
  def findArr[T](t: T) = {
    
    val hash = hashType match {
      case HashCode => t.hashCode
      case MD5 => md5(t)
      case SHA1 => sha1(t)
      case SHA256 => sha256(t)
      case MurmurHash3 => murmurHash3(t)
    }
    
    arr(math.abs(hash % hashSize)) 
  }
  
  def add(t: T) = {
    val ll = findArr(t)
    if (find(t) != None) false else {
      ll += t
      true
    }
  }
  
  def find[T](t: T) = {
    val ll = findArr(t)
    ll.find(x => x == t)
  }
  
  def remove(t: T) = {
    val ll = findArr(t)
    val llElem = ll.indexOf(t)
    if (llElem != -1) Some(ll.remove(llElem)) else None
  }
  
  def numCollisions[S >: T](s: S) = findArr(s).size - 1
  def collisions = for (i <- 0 until arr.size) println("Array " + i + " has " + (arr(i).size - 1) + " collisions")
  
  override def toString = (for (i <- arr; j <- i) yield j).toList.mkString(",")
  
}