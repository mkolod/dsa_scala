package us.marek.dsa.scala

import scala.util.Random

object BubbleSort extends App {
  
  val randomList = createRandomData(10)
  println(s"Unsorted list:\n$randomList\n")
  printf("Sorted list (bubble sort):\n%s", bubbleSort(randomList))
 
  def createRandomData(numEl: Int) = Array.fill(numEl)(Random.nextDouble).toList
  def sort(a: List[Double], b: Double) = if (a.last <  b) a ++ List(b) else a.init ++ List(b) ++ List(a.last)
  def bubbleStep(l: List[Double]) = l.sliding(1).reduceLeft((a,b) => sort(a, b(0)))   
  def bubbleSort(l: List[Double]) = l.foldLeft(l)((a,b) => bubbleStep(a))
 

}