package org.geo.scala.graph

final case class GraphVertex[T](name: T)
{
  override def equals(o: Any) = o match {
    case that: GraphVertex[T] => that.name.equals(this.name)
    case _                 => false
  }
  override def hashCode = name.hashCode
  override def toString = name.asInstanceOf[String] 
}