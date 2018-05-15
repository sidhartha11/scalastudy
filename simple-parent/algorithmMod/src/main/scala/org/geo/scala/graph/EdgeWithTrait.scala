package org.geo.scala.graph


//trait Edge2 [T,W] {
//  def weight: W
//  def either: T
//  def other(v: T) : T
//  def compare(that: T) : Int
//}
//object Edge2 {
//  def apply[T,W](v: T,w: T,weight: W)(ordering: Ordering[W]): Edge2[T,W] = 
//    new EdgeImpl2[T,W](v,w,weight)(ordering) 
// 
//
//private class EdgeImpl2[T,W](
//    private val v: T,
//    private val w: T,
//    private val weight: W)(implicit ordering: Ordering[W]) extends Edge2[T,W]  with Ordered[EdgeImpl2[T, W]]  {
//  
//}
//  def compare(that: T): Int = ???
//  def either: T = ???
//  def other(v: T): T = ???
//  def weight: W = ???
//
//}
//object EdgeImpl {
//  implicit def list2ordered[T,W](x: EdgeImpl[T, W])
//    (implicit elem2ordered: EdgeImpl[T, W] => Ordered[EdgeImpl[T, W]]): Ordered[EdgeImpl[T, W]] =
//  new Ordered[EdgeImpl[T, W]] {
//    //replace with a more useful implementation
//    def compare(that: EdgeImpl[T, W]): Int = {
//      0
//    }
//  }
//}
//
//trait Edge[T, W] {
//  def weightf: W
//  def other(v: T): T
//  def either: T
//}
//
//object Edge {
//  def apply[T , W ](v: T, w: T, weight: W): Edge[T, W] =
//    new EdgeImpl[T, W ](v, w, weight)
//case class EdgeImpl[T, W](
//  val v:      T,
//  val w:      T,
//  val weight: W)(implicit ordering: Ordering[W]) extends Ordered[EdgeImpl[T, W]] {
//  override def equals(o: Any) = o match {
//    case that: EdgeImpl[T, W] => that.v.equals(this.v)
//    case _                    => false
//  }
//  override def hashCode = v.hashCode
//  override def toString = "EdgeImpl: " + v + "," + w + "," + weight
//
//  def either: T = ???
//  def other(v: T): T = ???
//  def weightf: W = ???
//
//  /** Ordered implementation **/
//  def compare(that: EdgeImpl[T, W]) = {
//    if (that.weight == weight) {
//      0
//    } else if (ordering.gt(that.weight, weight)) {
//      -1
//    } else {
//      1
//    }
//
//  }
//
//}

//
//object EdgeImpl {
//  def apply[T,W <: Ordering[W]](v: T, w:T,weight: W): EdgeImpl[T,W] =
//    new EdgeImpl(v,w,weight)
//}

/** testing EdgeImpl **/
//object TestEdgeImpl {
//  def maxListImpParm[T](elements: List[T])(implicit ordering: Ordering[T]): T =
//
//    elements match {
//      case List() =>
//        throw new IllegalArgumentException("empty list!")
//      case List(x) => x
//      case x :: rest =>
//        val maxRest = maxListImpParm(rest)(ordering)
//        if (ordering.gt(x, maxRest)) x
//        else maxRest
//    }
//  def main(args: Array[String]) {
//    //    implicit val orderingEdge: Ordering[Edge[String,Double]] =
//    //      (v: String, w: String, wt: Double) => ???
//    val edge = new EdgeImpl[String, Double]("v1", "w1", 300)
//    println("edge = " + edge)
//    val listOfedges: List[EdgeImpl[GraphVertexGen[String, Int], Double]] =
//      List(
//        new EdgeImpl[GraphVertexGen[String, Int], Double](GraphVertexGen("v1", 100), GraphVertexGen("w1", 100), 300), new EdgeImpl[GraphVertexGen[String, Int], Double](GraphVertexGen("v1", 100), GraphVertexGen("w1", 100), 4), new EdgeImpl[GraphVertexGen[String, Int], Double](GraphVertexGen("v1", 100), GraphVertexGen("w1", 100), 322), new EdgeImpl[GraphVertexGen[String, Int], Double](GraphVertexGen("v1", 100), GraphVertexGen("w1", 100), 102))
//    println("lstOfedges=%s".format(listOfedges))
//    val mx = maxListImpParm[EdgeImpl[GraphVertexGen[String, Int], Double]](listOfedges)
//    println("mx = %s".format(mx))
//
//    println(listOfedges.sorted)
//
//  }
//}