package org.geo.scala.graph.undirected

case class GraphVertex(name: String, weight: Int, visited: Boolean) {
  override def equals(o: Any) = o match {
    case that: GraphVertex => that.name.equals(this.name)
    case _                 => false
  }
  override def hashCode = name.hashCode
  override def toString = name + ":" + weight + ":" + visited
}