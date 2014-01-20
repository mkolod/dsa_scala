package us.marek.dsa.scala.ds

import scala.annotation.tailrec

/**
 *  Find the median value of an array in O(n) time (includes demo)
 */
object FindMedian {

  /**
   * main method for demo
   */
  def main(args: Array[String]): Unit = {

    // print array contents
    def prettyPrint[T](a: Array[T]) = s"Array(${a.mkString(",")})"

    val arr1 = Array(1, 5, 4, 3, 2)
    val arr2 = arr1 :+ 6
    println(s"Array 1 (odd length): ${prettyPrint(arr1)}, median = ${median(arr1)}")
    println(s"Array 2 (even length): ${prettyPrint(arr2)}, median = ${median(arr2)}")
  }

  /**
   * Find median of an array in O(n) time
   * @param arr Array[T] where T can be viewed as Ordered[T]
   * @return list of medians - one in case of an odd-sized array
   *  and two in case of an even-sized array
   */
  def median[T <% Ordered[T]](arr: Array[T]): List[T] = {

    /**
     * recursive partitioning - the core of the QuickSelect algorithm
     * @see <a href="http://dl.acm.org/citation.cfm?doid=366622.366647">"Algorithm 65: find" by C.A.R. Hoare</a>
     * @param left
     * @param right
     * @param int
     */
    @tailrec   // tail recursive to reuse stack frame, prevent stack overflow and to speed things up
    def recPart(left: Int, right: Int, n: Int): T = {
      // we have one element - return immediately
      if (left == right) return arr(left)
      // get the index of the pivot 
      val pivotIdx = partition(arr, left, right, (left + right) / 2)
      if (pivotIdx == n) {
        /* If pivot index is equal to n, we found the desired element. 
         * This is because there will be n-1 elements smaller than the pivot.
         */
        arr(pivotIdx)
      } else if (n < pivotIdx) {
        /* If n is smaller than the pivot index, look in the left subarray, since 
           the pivot index indicates that there are more than n elements smaller
           than the pivot
         */
        recPart(left, pivotIdx - 1, n)
      } else {
        // If n is larger than the pivot index, look in the right subarray
        recPart(pivotIdx + 1, right, n)
      }
    }

    val len = arr.length
    /* If the array length is odd, pick the middle element
     * Return a list since in the even case we will be returning two elements
     * so we need to keep the return type consistent without risking its
     * generalization to Any.
     */
    if (len % 2 == 1) {
      List(recPart(0, len - 1, len / 2))
    } else {
      /* If the array length is even, pick the two elements closest to the middle
       * and return both of them in a list. 
       */
      List(recPart(0, len - 1, len / 2 - 1), recPart(0, len - 1, len / 2))
    }

  }

  /**
   * Method to partition the array (just like in Quicksort)
   * This is one implementation of the algorithm
   * (<a href="http://en.wikipedia.org/wiki/Quickselect">Wikipedia version</a>).
   * There are others, e.g. with the left and right indices moving towards the center, e.g.
   * <a href="http://www.scala-lang.org/docu/files/ScalaByExample.pdf">here</a>.
   * @param items array to be partitioned
   * @param left left index of the subarray to be partitioned
   * @param right right index of the subarray to be partitioned
   * @param piv index of the pivot's original position
   */
  private def partition[K <% Ordered[K]](items: Array[K], left: Int, right: Int, piv: Int): Int = {

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
        idx += 1
      }
    }
    swap(idx, right)
    idx
  }

}