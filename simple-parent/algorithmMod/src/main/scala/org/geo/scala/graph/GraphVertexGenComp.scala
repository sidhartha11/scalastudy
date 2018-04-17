package org.geo.scala.graph

/**
 * @author george
 * <pre>
 * This Class is not being used yet. Have to learn a little more
 * about scala before I can figure out how to create a 
 * comparable generic class like the ones we create in java.
 * So for now ... it is just a place holder ... :) 
 *</pre>
 * @param <T>
 * @param <W>
 */
final case class GraphVertexGenComp[T <: Ordered[T],W](name: T, weight: W)
extends Ordered[GraphVertexGenComp[T,W]]{
  override def equals(o: Any) = o match {
    case that: GraphVertexGenComp[T,W] => that.name.equals(this.name)
    case _                 => false
  }
  override def hashCode = name.hashCode
//  override def toString = name + ":" + weight + ":" + visited
  override def toString = name.asInstanceOf[String] + ":" + weight
  
    def compare(that: GraphVertexGenComp[T,W]) = {
    if (this.name < that.name ) {
      println("-1")
      -1
    } else if ( this.name > that.name ){
        println("1")
      1
    
    } else {
      println("0")
      0
    }
      
  }

}