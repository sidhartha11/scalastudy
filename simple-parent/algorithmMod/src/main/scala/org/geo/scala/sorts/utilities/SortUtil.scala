package org.geo.scala.sorts.utilities
object SortUtil {
  def swap[K](a: Array[K],x: Int, y: Int) = {
    val temp: K = a(x)
    a(x) = a(y)
    a(y) = temp
  }
  	def exchange[K](heap: Array[K],largest: Int,i:Int) = {
		val temp = heap(largest)
		heap(largest)=heap(i)
		heap(i)=temp
	}
}
