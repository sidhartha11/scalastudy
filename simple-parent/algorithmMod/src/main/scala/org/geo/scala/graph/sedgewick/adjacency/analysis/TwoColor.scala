package org.geo.scala.graph.sedgewick.adjacency.analysis

import scala.collection.mutable
import java.util.Stack
import org.geo.scala.graph.GraphConstants
import org.geo.scala.graph.sedgewick.GraphUtilitiesGen._
import org.geo.scala.graph.sedgewick.adjacency.Graph

/**
 * In the mathematical field of graph theory, a bipartite graph 
 * (or bigraph) is a graph whose vertices can be divided into two 
 * disjoint and independent sets {\displaystyle U} U and {\displaystyle V} 
 * V such that every edge connects a vertex in {\displaystyle U} U to one in 
 * {\displaystyle V} V. Vertex sets {\displaystyle U} U and {\displaystyle V} 
 * V are usually called the parts of the graph. Equivalently, 
 * a bipartite graph is a graph that does not contain any odd-length cycles.
 * @author george
 *
 * @param <T>
 */
trait TwoColor[T] {
  def isBipartite: Boolean
  def process(t: GraphConstants.Value): Unit
}
object TwoColor {
  def apply[T](graph: Graph[T]): TwoColor[T] =
    new TwoColorImpl[T](graph)

  /** private implementation **/
  private class TwoColorImpl[T](
    private val graph: Graph[T]) extends TwoColor[T] {

    println("instantiating TwoColorImpl")
    /** map used to determine is a vertex is connected to the input vertex,s **/
    private var marked: mutable.Map[T, Boolean] = _
    private var color: mutable.Map[T,Boolean] = _
    var isTwoColorable = true
    /**
     *         private implementations
     */
    private def DEBUG = true
    /** RECURSIVE VERSION **/
    private def dfsRecursive(graph: Graph[T], v: T): Unit = {
      marked(v) = true
      /** get the adjacent neighbors of v **/
      for (w <- graph.adj(v)) {
        /** if this neigbor has not been seen yet **/
        if (!hasPathTo(w)) {
          color(w) = !isColor(v) 
          /** check the adjacent neighors of w **/
          dfsRecursive(graph, w)
        } else if (isColor(w) == isColor(v)) {
          if (DEBUG) {
            println("w=%s, v=%s".format(w, v))
          }
          isTwoColorable = false
        }
      }
    }

    override def toString = "TwoColor:"
    /** PUBLIC API **/

    def isBipartite: Boolean = isTwoColorable

    def hasPathTo(v: T): Boolean = {
      val t = marked get v
      if (t == None) false
      else t get
    }
    
    def isColor(v: T): Boolean = {
      val t = color get v
      if (t == None) false
      else t get
    }

    def process(recurType: GraphConstants.Value): Unit = {
      /** disallow input of unknown vetex **/
      /** map used to determine is a vertex is connected to the input vertex,s **/
      println("recurType:" + recurType)
      marked = new mutable.HashMap[T, Boolean]()
      color  = new mutable.HashMap[T, Boolean]()
      
      /** initialize internal maps **/
//      for ((k, v) <- graph.getGraph) {
//        marked += (k -> false)
//        color += (k -> false)
//      }
      /**
       * Constructor Processing
       */
      var fun: (Graph[T], T) => Unit = null
      recurType match {
        case GraphConstants.recursive     => fun = dfsRecursive _
        case GraphConstants.non_recursive => fun = throw new IllegalArgumentException("iterative n/a")
      }

      for ((k, v) <- graph.getGraph) {
        if (!hasPathTo(k)) {
          fun(graph, k)
        }
      }
      /** debug code **/
      if (DEBUG) {
        for ((k, v) <- marked) {
          println(k + "," + v)
        }
        println(marked)
      }
    }
  }
}

object runner3 extends App {
  val graph = instantiateGraph[String, Int]("|")("kevinbacon.txt")
  val twoColor = TwoColor(graph)
  twoColor.process(GraphConstants.recursive)
  println("isBipartite %b".format(twoColor.isBipartite))
}
