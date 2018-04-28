package org.geo.scala.graph.sedgewick.adjacency.analysis.directed

import scala.collection.mutable
import java.util.Stack
import org.geo.scala.graph.sedgewick.GraphUtilitiesGen._
import org.geo.scala.graph.sedgewick.adjacency.Graph
import org.geo.scala.graph.GraphConstants

trait DirectedDFS[T] {

  def process(t: GraphConstants.Value)(v: T): Unit // find vertices in graph reachable from v
  def process(t: GraphConstants.Value,b:Boolean)(v: Iterable[T]): Unit // find vertices in graph reachable from set v
  def count: Int // how many vertices are connected to s?
  def marked(v: T): Boolean // is v connected to a source vertex
  // def pathTo(v: T): Iterable[T] // path from s to v
}

object DirectedDFS {
  def DEBUG = false
  def apply[T](graph: Graph[T]): DirectedDFS[T] =
    new DirectedDFSImpl[T](graph)

  /** private implementation **/
  private class DirectedDFSImpl[T](
    private val graph: Graph[T]) extends DirectedDFS[T] {
    private var recurTyp: GraphConstants.Value = _
    println("instantiating DirectedDFSImpl")
    /** map used to determine is a vertex is connected to the input vertex,s **/
    private var mark: mutable.Map[T, Boolean] = _

    /** count of all vertices connected to input vertex s **/
    var counter = 0

    /**
     *         private implementations
     */
    private def dfsRecursive(graph: Graph[T], v: T): Unit = {
      mark(v) = true
      if ( DEBUG ) {
      println("mark=%s".format(mark))
      }
      /** count each connected vertex **/
      counter += 1
      /** get the adjacent neighbors of v **/
      for (w <- graph.adj(v)) {
        /** if this neigbor has not been seen yet **/
        if ((mark get w) == None) {
          /** check the adjacent neighors of w **/
          dfsRecursive(graph, w)
        }
      }
    }

    private def dfs(graph: Graph[T], v: T): Unit = {
      val stack = mutable.Stack[T]()
      /** push the first element onto the stack **/
      stack.push(v)
      while (!stack.isEmpty) {
        val s = stack.pop()
        if ((mark get s) == None) {
          counter += 1
          mark(s) = true
        }

        /** get all the adjacent vertices of s **/
        /** push each one that is not mark onto the stack **/
        for (w <- graph.adj(s)) {
          if ((mark get w) == None) {
            stack.push(w)
          }
        }
      }
    }

    override def toString = "DirectedDFS:" + counter

    /**
     *          public api implementation
     */

    /**
     * @return the number of verticies connect to input
     * source vertex
     */
    def count: Int = counter

    def marked(v: T): Boolean = {
      val t = mark get v
      if (t == None) false
      else t get
    }

    /**
     * find vertices in graph that are reachable by s.
     * @param recurType
     * @param s
     */
    def process(recurType: GraphConstants.Value)(s: T): Unit = {
      /** disallow input of unknown vetex **/
      require(requireVertexInGraph(graph, s), "vertex not in graph")

      /** map used to determine is a vertex is connected to the input vertex,s **/
      mark = new mutable.HashMap[T, Boolean]()

      /** count of all vertices connected to input vertex s **/
      counter = 0
      /**
       * Constructor Processing
       */
      recurTyp = recurType
      recurTyp match {
        case GraphConstants.recursive     => dfsRecursive(graph, s)
        case GraphConstants.non_recursive => dfs(graph, s)
      }

      /** debug code **/
      if (DEBUG) {
        for ((k, v) <- mark) {
          println(k + "," + v)
        }
        println(mark)
      }
    }
    
    /**
     * find vertices in graph that are reachable by elements in set
     * @param recurType
     * @param set
     */
    def process(recurType: GraphConstants.Value,b:Boolean)(set: Iterable[T]): Unit = {
      /** disallow input of unknown vetex **/
      

      /** map used to determine is a vertex is connected to the input vertex,s **/
      mark = new mutable.HashMap[T, Boolean]()

      /** count of all vertices connected to input vertex s **/
      counter = 0
      /**
       * Constructor Processing
       */
      for ( s <- set if !marked(s) ) {
      recurType match {
        case GraphConstants.recursive     => dfsRecursive(graph, s)
        case GraphConstants.non_recursive => dfs(graph, s)
      }
      }
      /** debug code **/
      if (DEBUG) {
        for ((k, v) <- mark) {
          println(k + "," + v)
        }
        println(mark)
      }
    }
  }

}