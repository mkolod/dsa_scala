package us.marek.dsa.scala.ds

import scala.collection.mutable.BitSet
import scala.io.Source
import math.{exp, log, pow, round}
  // get enum values (Space, FalsePosRate)

object BloomFilter extends App {
  
  println("Bloom filter test\n")
  
  /* data is from the Unix dictionary - generate one yourself for persistence using
     $cat /usr/share/dict/words > dictionary.txt
     this program will simply get it from the existing Unix file descriptor
   */
  
  val data = Source.fromFile("/usr/share/dict/words").getLines().toList
  val numChar = data.map(_.length).sum + data.size - 1
  /* the necessary bit count was estimated - the constructor will throw an exception
     if the bit count, the data size and the positive error rate are inconsistent
   */
  val bloom = new BloomFilter[Int](30000000, data.size, 1e-5, Space)
  data.foreach(i => bloom.insert(i))
  data.foreach(i => println(s"filter contains '$i'? ${bloom.contains(i)}"))
  println()
  List("foo1", "foo2", "foo3").foreach(i => println(s"filter contains '$i'? ${bloom.contains(i)}"))
  println(f"\nfalse positive rate = ${bloom.falsePositiveRate}%g")
  println(s"\nBits used = ${bloom.bits}")
  println(s"\nRaw string storage would have used $numChar in a single byte array, and more in a String array and a hash table")
  println(s"\nNumber of elements in filter = ${bloom.actualNumEl}")
  println(s"\nhash function count = ${bloom.hashCt}")
  
}


trait BloomFilterOptimization
case object Space extends BloomFilterOptimization
case object FalsePosRate extends BloomFilterOptimization 


class BloomFilter[+T](val maxBits: Int, val expElems: Int,
    val maxFalsePosRate: Double, val optimization: BloomFilterOptimization) {

  require(falsePositiveRate(expElems, maxBits) <= maxFalsePosRate,
    s"False positive rate too high for expected $expElems elements even using $maxBits bits\n" +
    s"Need at least ${neededBits(expElems, maxFalsePosRate)} bits"
  )
  
  /* if optimizing for space, use just as many bits as needed to ensure the max false positive rate,
     if optimizing for false positive rate, use maxBits
   */
  val bits = optimization match {
    case Space => neededBits(expElems, maxFalsePosRate)
    case FalsePosRate => maxBits
  }
  
  val hashCt = numHashes(bits, expElems)
  
  private[this] val bitSet = new BitSet(bits)
  
  /* synchronized because Scala's internal solution seems stateful
     and we may wish to have concurrent mutability - sad but true
   */
  private[this] def hash[S >: T](data: S, seed: Int) = synchronized {
    import scala.util.MurmurHash._
    /* had to copy the magic numbers from scala.util.Murmurhash since
       they're private and I wanted to implement this here 
       the Scala API now suggests MurmurHash3 but my version on this machine
       didn't have it yet
     */
    val seedArray = 0x3c074a61
    val hiddenMagicA = 0x95543787
    val hiddenMagicB = 0x2ad7eb25
    val h = startHash(seed * seedArray)
    // .## is shorthand for the hashCode method
    finalizeHash(extendHash(h, data.##, hiddenMagicA, hiddenMagicB))
  }
  
  /* number of elements inserted - to check the current false positive rate,
     can even throw an exception while adding new elements if the max false 
     pos rate is exceeded, or report the issue to the user by returning 
     some wrapped value
   */
  private[this] var numEl = 0
  
  def actualNumEl = numEl // estimated numEl based on the bit state to be implemented
  
  private[this] def numHashes(bits: Int, elems: Int) = round(log(2.0) * bits / elems) 
  
  private[this] def falsePositiveRate(elems: Int, bits: Int): Double = {
    val hashes = numHashes(bits, elems)
    math.pow((1 - exp(-1.0*hashes*elems/bits)), hashes)
  }
  
  def falsePositiveRate: Double = falsePositiveRate(numEl, bits)
    
  private[this] def neededBits(expElems: Int, falePosRate: Double) = 
    -math.ceil((expElems * log(falePosRate)) / pow(log(2.0), 2)).toInt
  
  def insert[S >: T](element: S) = {
   getHashes(element).foreach(h => bitSet += h) 
   numEl += 1
  }
  
  def contains[S >: T](element: S) = {
    getHashes(element).forall(i => bitSet.contains(i))
  }
  
  private[this] def getHashes[S >: T](element: S) = {
    /* need to figure out how to get rid of abs
     murmurhash mainly does the avalanche given the seed
     so far the filter's accuracy is unaffected, but it's a bit hacky
     */
    (1 to hashCt.toInt).map(seed => math.abs(hash(element, seed) % bits))
  }
 
}