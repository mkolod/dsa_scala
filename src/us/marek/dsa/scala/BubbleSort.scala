package us.marek.dsa.scala

import scala.util.Random

/**
 * Note that this is a particularly naive implementation - it does N^2 comparisons instead of the
 * theoretically optimal N*(N-1)/2, but it's functional, concise, and of course still O(N^2).
 * A future implementation will include the optimization to N*(N-1)/2 comparisons.
 */

object BubbleSort extends App {
  
  val randomList = createRandomData(10)
  println(s"Unsorted list:\n$randomList\n")
  printf("Sorted list (bubble sort):\n%s", bubbleSort(randomList))
 
  def createRandomData(numEl: Int) = Array.fill(numEl)(Random.nextDouble).toList
  def sort(a: List[Double], b: Double) = if (a.last <  b) a ++ List(b) else a.init ++ List(b) ++ List(a.last)
  def bubbleStep(l: List[Double]) = l.sliding(1).reduceLeft((a,b) => sort(a, b(0)))   
  def bubbleSort(l: List[Double]) = l.foldLeft(l)((a,b) => bubbleStep(a))
 

}