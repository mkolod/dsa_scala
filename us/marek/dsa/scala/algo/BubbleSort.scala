package us.marek.dsa.scala.algo

object BubbleSort extends App {
  
  val randomList = Utils.createRandomData(10)
  
  println(s"Unsorted list:\n$randomList\n")
  printf("Sorted list (bubble sort):\n%s\n", bubbleSort(randomList, randomList.length))

  def sort(a: List[Double], b: Double) = 
    if (a.last <  b) a ++ List(b)
    else a.init ++ List(b) ++ List(a.last)
    
  def bubbleStep(l: List[Double]) = l.sliding(1).reduceLeft((a,b) => sort(a, b(0)))   

  def bubbleSort(l: List[Double], unsortedElems: Int): List[Double] =
    unsortedElems match {
    
      case i:Int if (i > 1) => 
        bubbleSort(bubbleStep(l.take(unsortedElems)), unsortedElems - 1) ++
          l.drop(unsortedElems)
        
      case _ => l
    }
}