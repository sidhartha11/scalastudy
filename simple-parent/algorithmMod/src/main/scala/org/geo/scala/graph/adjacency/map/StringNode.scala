package org.geo.scala.graph.adjacency.map

case class StringNode(name: String, weight: Int, visited: Boolean) {

    override def equals(o: Any) = o match {
    case that: StringNode => that.name.equals(this.name)
    case _ => false
  }
  override def hashCode = name.hashCode
  
override def toString = name +":"+weight+":"+ visited
}