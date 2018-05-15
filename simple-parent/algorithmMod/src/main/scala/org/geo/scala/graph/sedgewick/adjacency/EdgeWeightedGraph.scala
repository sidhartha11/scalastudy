package org.geo.scala.graph.sedgewick.adjacency
import scala.collection.mutable
import org.geo.scala.graph.GraphConstants
import org.geo.scala.graph.GraphVertexGen
import org.geo.scala.graph.TestTrait

/**
 * @author George Curington
 * @version 1.0
 * @since 4/14/2018
 * <pre>
 * trait EdgeWeightedGraph[T]
 *
 * This is a trait. It is used to give a public interface
 * to users the Edge Weighted Adjacency Structure.
 * This version of the adjacency structure is based solely on
 * hash maps.
 * The list of vertices is a Map containing a key for
 * the vertex and a value for the list of neighbor vertices.
 * The list of neighbor vertices is also a Map. There is some
 * additional space used to represent the vertices since it is
 * not necessary to contain a complete vertex object as the key.
 * But this implementation does just that. The vertex object, represented
 * buy the type "T" generic parameter can be any class object that
 * overrided hashcode and equals.
 *
 * </pre>
 */
object Order {
  def compare[T: Ordering] (o1:T, o2:T):Boolean = {
  val ord = implicitly[Ordering[T]] 
  import ord.mkOrderingOps
  o1 > o2
}
}
trait EdgeWeightedGraph[T, W] {
  def addEdge(edge: TestTrait[T, W]): Unit // add edge v-w to this graph
  def edges: Iterable[(T, mutable.LinkedHashSet[TestTrait[T, W]])]
  def V: Int // number of vertices
  def E: Int // number edges
  def adj(v: T) : mutable.LinkedHashSet[TestTrait[T,W]]
  
}

object EdgeWeightedGraph {

//trait Ordering[T] {
//  def compare(x: T, y: T): T
//}
//
//implicit object DoubleOrdering extends Ordering[Double] {
//  def compare(x: Double, y: Double): Double = {
//    println("IM HERE DO")
//    val t =  x - y
//    if ( t == 0 ) {
//      x 
//    } else if ( t > 0 ) {
//      y
//    } else { 
//      x
//    }
//  }
//}
  
  import Order._
  
  def apply[T, W](biDir: GraphConstants.Value = GraphConstants.directed): EdgeWeightedGraph[T, W] =
    new EdgeWeightedGraphImpl[T, W](biDir)

  private class EdgeWeightedGraphImpl[T, W](

    private val biDir: GraphConstants.Value) extends EdgeWeightedGraph[T, W] {

    /** Map Of Edges **/
    /**
     * Each element of the edgeMap must contain a set of edges
     */
    //    private val edgeMap: mutable.LinkedHashMap[T, TestTrait[T,W]] = new mutable.LinkedHashMap[T, TestTrait[T,W]]()
    private val edgeMap: mutable.LinkedHashMap[T, mutable.LinkedHashSet[TestTrait[T, W]]] = new mutable.LinkedHashMap[T, mutable.LinkedHashSet[TestTrait[T, W]]]()

    /** number of vertices in the graph **/
    private[this] var numberVertices = 0
    private[this] var numberEdges = 0
    /** DISPLAY DIRECTION **/
    println("mapped adj " + biDir)

    private def DEBUG = false 
    private def directedInsertion(e: TestTrait[T, W]): Unit = {
      throw new IllegalArgumentException("Graph Must Be Undirected For Weighted Impl")
    }

    private def updateMap(v: T, e: TestTrait[T, W]) {
      if ( DEBUG )
      println("adding edge %s to vertex %s".format(e, v))
      val g = edgeMap get v
      if (g == None) {
        var m = mutable.LinkedHashSet[TestTrait[T, W]]()
        m += e
        edgeMap += (v -> m)
      } else {
        g.get += e
      }
    }
    private def undirectedInsertion(e: TestTrait[T, W]): Unit = {
      val v = e.either
      val w = e.other(v)
      if ( DEBUG ) 
      println("v = %s , w = %s".format(v, w))
      /** update edges in map **/
      updateMap(v, e)
      updateMap(w, e)
    }

    /**
     * returns the number of vertices
     * @see org.geo.scala.graph.undirected.EdgeWeightedGraph#V()
     */
    def V: Int = numberVertices

    /**
     * returns the number of edges
     * @see org.geo.scala.graph.undirected.EdgeWeightedGraph#E()
     */
    def E: Int = {
      biDir match {
        case GraphConstants.undirected => numberEdges / 2
        case GraphConstants.directed   => numberEdges
      }
    }
//  def qSort[K: Ordering](a: Array[K]): Unit = {
    def edges[TestTrait[T,W]: Ordering](ord: Ordering[TestTrait[T,W]]): Iterable[(T, mutable.LinkedHashSet[TestTrait[T, W]])] = {
      val set: mutable.LinkedHashSet[TestTrait[T,W]] = mutable.LinkedHashSet[TestTrait[T, W]]()

      for {
        v <- edgeMap.keySet
        e <- edgeMap.get(v) 
//        a <- e if a.other(v) > v 
        a <- e if ord.compare(a.other(v),v)
      } {
        set += a
      }
      
      edgeMap.toIterable
    }

    
    def adj(v: T): mutable.LinkedHashSet[TestTrait[T,W]] = {
      val g = edgeMap get v
      if ( g == None ) 
        mutable.LinkedHashSet.empty[TestTrait[T, W]]
      else
        g get
    }
    def addEdge(edge: TestTrait[T, W]): Unit = {
      /** if biDir then must update both as neighbors **/
      biDir match {
        case GraphConstants.undirected => undirectedInsertion(edge)
        case GraphConstants.directed   => directedInsertion(edge)
      }
    }
  }
}