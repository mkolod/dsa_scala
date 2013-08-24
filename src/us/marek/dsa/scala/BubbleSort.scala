package us.marek.dsa.scala

import scala.util.Random

object BubbleSort extends App {
  
  val randomList = createRandomData(10)
  println(s"Unsorted list:\n$randomList\n")
  printf("Sorted list (bubble sort):\n%s", bubbleSort(randomList))
 
  def createRandomData(numEl: Int) = Array.fill(numEl)(Random.nextDouble).toList
  def sort(a: List[Double], b: List[Double]) = if (a.last <  b(0)) a ++ b else a.init ++ b ++ List(a.last)
  def bubbleStep(l: List[Double]) = l.sliding(1).reduceLeft(sort)   
  def bubbleSort(l: List[Double]) = l.foldLeft(l)((a,b) => bubbleStep(a))
 

}