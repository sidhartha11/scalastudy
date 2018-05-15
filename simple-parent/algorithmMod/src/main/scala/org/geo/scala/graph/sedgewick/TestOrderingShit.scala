package org.geo.scala.graph.sedgewick

object TestInside {
trait Ordering[T] {
  def compare(x: T, y: T): T
}


implicit object DoubleOrdering extends Ordering[Double] {
  def compare(x: Double, y: Double): Double = {
    println("IM HERE DO")
    val t =  x - y
    if ( t == 0 ) {
      x 
    } else if ( t > 0 ) {
      y
    } else { 
      x
    }
  }
}

implicit object IntOrdering extends Ordering[Int] {
  def compare(x: Int, y: Int): Int = {
    println("IM HERE IO")
    val t =  x - y
    if ( t == 0 ) {
      x 
    } else if ( t > 0 ) {
      y
    } else { 
      x
    }
  }
}



}

object OtherObject {
  import TestInside._
  def min[T](a: T, b: T)(implicit o:Ordering[T]) = {
    println("in min")
    val t = o.compare(a,b)
    t
//    if ( t == 0 ) a
//    else if ( t < 0 ) 
//      a
//    else 
//      b
  } 
  
    def max[T](a: T, b: T)(implicit o:Ordering[T]) = {
    println("in max")
    val t = o.compare(a,b)
    t
//    if ( t == 0 ) a
//    else if ( t < 0 ) 
//     b
//    else 
//     a
  } 
  def main(args:Array[String]) {
    val d1:Double = 4.0
    val d2:Double = 2.0 
//    println(min("4","2"))
    println(min(4,2))
    println(min(d1,d2))
  }
  
}