package us.marek.dsa.scala

/**
 * 
 */
object Utils {
  
  import scala.util.Random
  
  def createRandomData(numEl: Int) = Array.fill(numEl)(Random.nextDouble).toList
  
  def findMinIndex(l: List[Double]) = l.zipWithIndex.min._2

}