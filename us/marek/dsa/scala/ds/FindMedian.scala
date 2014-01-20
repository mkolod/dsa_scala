package us.marek.dsa.scala.ds

import scala.annotation.tailrec

/**
 *  Demo - main
 */
object FindMedian {

  def main(args: Array[String]): Unit = {

    def prettyPrint[T](a: Array[T]) = s"Array(${a.mkString(",")})"

    val arr1 = Array(1, 5, 4, 3, 2)
    val arr2 = arr1 :+ 6
    println(s"Array 1 (odd length): ${prettyPrint(arr1)}, median = ${median(arr1)}")
    println(s"Array 2 (even length): ${prettyPrint(arr2)}, median = ${median(arr2)}")
  }

  def median[T <% Ordered[T]](arr: Array[T]) = {

    @tailrec
    def recPart(left: Int, right: Int, n: Int): T = {
      if (left == right) return arr(left)
      val pivotIdx = partition(arr, left, right, (left + right) / 2)
      if (pivotIdx == n) {
        arr(pivotIdx)
      } else if (n < pivotIdx) {
        recPart(left, pivotIdx - 1, n)
      } else {
        recPart(pivotIdx + 1, right, n)
      }
    }

    val len = arr.length
    if (len % 2 == 1) {
      List(recPart(0, len - 1, len / 2))
    } else {
      List(recPart(0, len - 1, len / 2 - 1), recPart(0, len - 1, len / 2))
    }

  }

  def partition[K <% Ordered[K]](items: Array[K], left: Int, right: Int, piv: Int): Int = {

    def swap(i: Int, j: Int): Unit = {
      val temp = items(i)
      items(i) = items(j)
      items(j) = temp
    }

    if (left == right) return left
    val pivVal = items(piv)
    swap(piv, right)
    var i = left
    var idx = left
    for (i <- (left to right - 1)) {
      if (items(i) < pivVal) {
        if (i != idx) swap(i, idx)
        idx = idx + 1
      }
    }
    swap(idx, right)
    idx
  }

}