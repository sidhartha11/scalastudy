package org.geo.scala.graph.sedgewick.adjacency.analysis

import scala.collection.mutable
import java.util.Stack
import org.geo.scala.graph.GraphConstants
import org.geo.scala.graph.sedgewick.GraphUtilitiesGen._
import org.geo.scala.graph.sedgewick.adjacency.Graph

/**
 * Determine if the input graph ( adj map ) contains
 * a cycle i.e. is acyclic
 * @author george
 *
 * @param <T>
 */
trait Cycle[T] {
  def isCyclic: Boolean
  def process(t: GraphConstants.Value): Unit
}
object Cycle {
  def apply[T](graph: Graph[T]): Cycle[T] =
    new CycleImpl[T](graph)

  /** private implementation **/
  private class CycleImpl[T](
    private val graph: Graph[T]) extends Cycle[T] {

    println("instantiating DepthFirstPathsImpl")
    /** map used to determine is a vertex is connected to the input vertex,s **/
    private var marked: mutable.Map[T, Boolean] = _
    var hasCycle = false
    /**
     *         private implementations
     */
    private def DEBUG = false
    /** RECURSIVE VERSION **/
    private def dfsRecursive(graph: Graph[T], v: T, u: T): Unit = {
      marked(v) = true
      /** get the adjacent neighbors of v **/
      for (w <- graph.adjreverse(v)) {
        /** if this neigbor has not been seen yet **/
        if (!hasPathTo(w)) {
          /** check the adjacent neighors of w **/
          dfsRecursive(graph, w, v)
        } else if (w != u) {
          if (DEBUG) {
            println("w=%s, u=%s".format(w, u))
          }
          hasCycle = true
        }
      }
    }

    override def toString = "Cycle:"
    /** PUBLIC API **/

    def isCyclic: Boolean = hasCycle

    def hasPathTo(v: T): Boolean = {
      val t = marked get v
      if (t == None) false
      else t get
    }

    def process(recurType: GraphConstants.Value): Unit = {
      /** disallow input of unknown vetex **/
      /** map used to determine is a vertex is connected to the input vertex,s **/
      println("recurType:" + recurType)
      marked = new mutable.HashMap[T, Boolean]()
      /**
       * Constructor Processing
       */
      var fun: (Graph[T], T, T) => Unit = null
      recurType match {
        case GraphConstants.recursive     => fun = dfsRecursive _
        case GraphConstants.non_recursive => fun = throw new IllegalArgumentException("iterative n/a")
      }

      for ((k, v) <- graph.getGraph) {
        if (!hasPathTo(k)) {
          fun(graph, k, k)
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

object runner2 extends App {
  val graph = instantiateGraph[String, Int](",")(CITIES_NOCYCLE)
  val cycle = Cycle(graph)
  cycle.process(GraphConstants.recursive)
  println("isCyclic %b".format(cycle.isCyclic))
}

