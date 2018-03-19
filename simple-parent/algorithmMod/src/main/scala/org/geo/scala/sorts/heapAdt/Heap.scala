package org.geo.scala.sorts.heapAdt
import org.geo.scala.sorts.utilities.SortUtil.exchange


/**
 * @author george Curington
 * Implementation of a max heap algorithm in Scala.
 * Converted from Java Program.
 * Complexity is O(logn) for performing certain important tasks:
 * <p>
 * Operations having optimal performance:
 * 
 * Find max    Delete max    insert key    increase key    decrease key
 * O(1)        O(log n)      O( log n )    O(log n)        O( log n) 
 * 
 *  
 * properties: 
 *         1. height of a complete heap = number of
 *         edges from the root to the leaf. The max if there are multiple paths.
 *         2. given the height, what are the maximum number of nodes present in
 *         a complete binary tree ( heap )
 * 
 *         2 power(h+1) - 1 ( 2^h+1 -1 )
 *
 *         note: for a ternary tree we get ( 3^h+1 - 1 )/ 2
 * 
 *         3. given a complete( or almost complete ) binary tree with n nodes, what is
 *         the height log n E.G. n = number of nodes so height = log n
 * 
 *         4. An array that is already sorted is already a max heap.
 * 
 *         5. leafs: ( floor(n/2) + 1 ) to n
 * 
 *         6. number nodes in a tree of height h n / ( 2^(h+1) )
 * 
 * 
 *         Time Complexity to Build a Max/Min Heap: O(n) Space Complexity O
 *         (logn)
 * 
 *         NOTE: This algorithm is translating indexes from starting point of 1
 *         to 0.
 * </p>
 *
 */
object Heap {
  var heap_size = 0
  def leftI(i: Int) = (i << 1) - 1
  def rightI(i: Int) = ((i << 1) + 1) - 1
  def build_heap[K: Ordering](heap: Array[K]): Unit = {

    /**
     * Internal function to max heapify the heap 
     */
    def max_heapify(heap: Array[K], i: Int, ord: Ordering[K]): Unit = {
      val l = leftI(i)
      val r = rightI(i)
      var largest = 0
      if (l <= heap_size && ord.compare(heap(l), heap(i - 1)) > 0) {
        largest = l
      } else {
        largest = i - 1
      }

      if (r <= heap_size && ord.compare(heap(r), heap(largest)) > 0) {
        largest = r
      }

      if (largest != i - 1) {
        exchange(heap, largest, i - 1)
        max_heapify(heap, largest + 1, ord)
      }
    }
    /**
     * This section repeatedly calls max_heapify to convert the array into
     * max heap.
     */
    heap_size = heap.length-1
    val d = (Math.floor((heap_size + 1) / 2)).asInstanceOf[Int]
    for (i <- d to 1 by -1 ) {
      max_heapify(heap, i, implicitly[Ordering[K]])
    }
  }
}
object TestHeap extends App {
  import Heap._
  val a = Array(4,5,3,7,8,7,90,2)
  build_heap(a)
  println(a.mkString("\n"))
}