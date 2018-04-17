package org.geo.scala.graph

final case class GraphVertexGen[T, W](name: T, weight: W)
{
  override def equals(o: Any) = o match {
    case that: GraphVertexGen[T,W] => that.name.equals(this.name)
    case _                 => false
  }
  override def hashCode = name.hashCode
//  override def toString = name + ":" + weight + ":" + visited
  override def toString = name.asInstanceOf[String] + ":" + weight
}