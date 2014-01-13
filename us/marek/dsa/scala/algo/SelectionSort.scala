package us.marek.dsa.scala.algo

object SelectionSort extends App {
  
  val randomList = Utils.createRandomData(10)
  
  println(s"Unsorted list:\n$randomList\n")
  printf("Sorted list (bubble sort):\n%s\n", selectionSort(randomList, 0))

  def sortStep(l: List[Double]) = {
    
    val minI = Utils.findMinIndex(l)
    
    minI match {
      
      case i: Int if (i > 0) => l(minI) :: l.take(minI).tail ++ List(l(0)) ++ l.drop(minI + 1)
      case _ => l
    }
  }
  

  def selectionSort(l: List[Double], sortedElems: Int): List[Double] = sortedElems match {
    
    case i: Int if (i < l.length) =>  selectionSort(l.take(sortedElems) ++ sortStep(l.drop(sortedElems)), sortedElems + 1)
    case _ => l
  }
  
}