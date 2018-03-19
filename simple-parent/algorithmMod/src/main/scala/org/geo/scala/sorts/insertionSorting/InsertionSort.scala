package org.geo.scala.sorts.insertionSorting

/**
 * @author george
 * <pre>
 * Java conversion of insertionSort to SCALA
 * complexity O(n^2)
 * </pre>
 *
 */
object InsertionSort {
   def iSort[K: Ordering](a: Array[K]): Unit = {
     for ( i <- 1 to a.length-1) {
       val key: K = a(i)
       var j = i -1     
       while ( j > -1 && (implicitly[Ordering[K]]).compare(a(j),key) > 0 ){
         a(j+1)=a(j)
         j=j-1
       }
       a(j+1) = key
     }
}
}