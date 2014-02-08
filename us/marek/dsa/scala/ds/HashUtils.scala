package us.marek.dsa.scala.ds

import java.io.ByteArrayOutputStream
import java.io.ObjectOutputStream
import java.io.ObjectInputStream
import java.io.ByteArrayInputStream
import java.security.MessageDigest
import java.math.BigInteger

import scala.util.hashing.MurmurHash3

object HashUtils extends App {
  
  def murmurHash3(obj: Any): Int = murmurHash3(serializeObject(obj))
  
  def murmurHash3(arr: Array[Byte]): Int = MurmurHash3.bytesHash(arr)
  
  def md5(obj: Any): Int = md5(serializeObject(obj))
  
  def md5(arr: Array[Byte]): Int = digest(arr, "MD5")
 
  def sha1(obj: Any): Int = sha1(serializeObject(obj))
  
  def sha1(arr: Array[Byte]): Int = digest(arr, "SHA-1")
  
  def sha256(obj: Any): Int = sha256(serializeObject(obj))
  
  def sha256(arr: Array[Byte]): Int = digest(arr, "SHA-256")
  
  private def digest(arr: Array[Byte], hash: String): Int = {
    val md = MessageDigest.getInstance(hash)
    val digest = md.digest(arr)
    BigInt(digest).toInt
  }
  
  def serializeObject(obj: Any): Array[Byte] = {
    val bos = new ByteArrayOutputStream()
    val oos = new ObjectOutputStream(bos)
    oos.writeObject(obj)
    oos.close()
    bos.toByteArray()
  }
  
  def deserializeObject(arr: Array[Byte]): Any = {
    val bis = new ByteArrayInputStream(arr)
    val ois = new ObjectInputStream(bis)
    val obj = ois.readObject()
    ois.close()
    obj
  }

}