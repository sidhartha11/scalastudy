package org.geo.scala.sorts.quicksorting
import org.geo.scala.sorts.utilities.SortUtil.swap
/**
 * @author george
 * <pre>
 * quick sort algorithm converted from java to scala. This is an 
 * in place sort.
 * O(nlogn) best case time complexity
 * O(n2) worst case.
 * This sort function expects the elements of the Array being sorted to
 * extend the ordered trait.
 * </pre>
 *
 */
object QuickSort {
  def qSort[K: Ordering](a: Array[K]): Unit = {
    
    def sort(a: Array[K], start: Int, end: Int, ord: Ordering[K]): Unit = {

      /** partition for partitioning the array about a pivot point **/
      def partition(a: Array[K], start: Int, end: Int, ord: Ordering[K]): Int = {
        val pivot: K = a(end)
        var pIndex: Int = start - 1
        for (i <- start to end) {
          if (ord.compare(a(i), pivot) < 0) {
            //       if ( a(i) < pivot ) {
            pIndex = pIndex + 1
            swap(a,i,pIndex)
          }
        }
        swap(a,end,pIndex + 1)
        pIndex + 1
      }
      /** body of sort **/
      if (start < end) {
        val partitionIndex: Int = partition(a, start, end, ord)
        sort(a, start, partitionIndex - 1, ord)
        sort(a, partitionIndex + 1, end, ord)
      }
    }
    /** body of quickSort **/
    sort(a, 0, a.length - 1, implicitly[Ordering[K]])
  }
}