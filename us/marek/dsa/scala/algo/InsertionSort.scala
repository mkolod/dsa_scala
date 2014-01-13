package us.marek.dsa.scala.algo

object InsertionSort extends App {

  val randomList = Utils.createRandomData(10)
  
  println(s"Unsorted list:\n$randomList\n")
  printf("Sorted list (insertion sort):\n%s\n", insertionSort(randomList))
  
  def insert(l: List[Double], d: Double) = l.takeWhile(_ < d) ++ List(d) ++ l.dropWhile(_ < d) 
  def insertionSort(l: List[Double]) = l.foldLeft(List[Double]())(insert)
  
}