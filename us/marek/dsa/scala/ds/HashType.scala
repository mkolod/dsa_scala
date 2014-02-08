package us.marek.dsa.scala.ds

object HashType extends Enumeration {
  type HashType = Value
  val MD5, SHA1, SHA256, MurmurHash3, HashCode = Value
}