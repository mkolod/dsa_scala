package us.marek.dsa.scala.algo

import scala.util.Random

object MegeSort extends App {
  
  val rand = new Random(1)
  val random = List.fill(10)(rand.nextInt(100))
  val sorted = mergeSort(random)
  
  println(s"random = $random")
  println(s"sorted = $sorted")
  
  
  def mergeSort[T <% Ordered[T]](data: List[T]) = {
    
    def merge(xs: List[T], ys: List[T]): List[T] = (xs, ys) match {
      case (Nil, _) => ys
      case (_, Nil) => xs
      case (x :: xs1, y :: ys1) => 
        if (x < y) x :: merge(xs1, ys) else y :: merge(xs, ys1)        
    }
    
    def sort(xs: List[T]): List[T] = xs.size / 2 match {
      case 0 => xs
      case mid: Int => merge(sort(xs.take(mid)), sort(xs.drop(mid)))
    }
   
    sort(data)
  }
}