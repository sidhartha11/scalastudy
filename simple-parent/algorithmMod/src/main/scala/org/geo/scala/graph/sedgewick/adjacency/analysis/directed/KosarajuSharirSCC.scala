package org.geo.scala.graph.sedgewick.adjacency.analysis.directed

import scala.collection.mutable
import org.geo.scala.graph.GraphConstants
import org.geo.scala.graph.sedgewick.GraphUtilitiesGen._

import org.geo.scala.graph.sedgewick.adjacency.Graph
import scala.collection.mutable.Queue

trait KosarajuSharirSCC[T] {
  /** find all paths from source s **/
  def process(t: GraphConstants.Value): Unit
  def stronglyConnected(v: T, w: T): Boolean // are v and w connected ?
  def count: Int // number of connected components
  def id(v: T): Option[Int] // component identifier for v, key in adj map
}

object KosarajuSharirSCC {
  def apply[T](graph: Graph[T]): KosarajuSharirSCC[T] =
    new KosarajuSharirSCCImpl[T](graph)

  /** private implementation **/
  private class KosarajuSharirSCCImpl[T](
    private val graph: Graph[T]) extends KosarajuSharirSCC[T] {

    println("instantiating DepthFirstPathsImpl")
    /** map used to determine is a vertex is connected to the input vertex,s **/
    private var marked: mutable.Map[T, Boolean] = _
    /** map used to store pointers to directly connected components **/
    private var idV: mutable.Map[T, Int] = _
    private var counter = 0

    var dcounter = 0
    /** used for tracing and debugging **/
    var start = 0l
    /** used for tracing and debugging **/
    /**
     *         private implementations
     */
    private def DEBUG = false

    /**
     * Two versions of a depth first search of a graph used
     * to determine the path between a source and destination
     * vertex in a graph. This works with both undirected and
     * directed graphs. This version deviates from the classical
     * versions given in most texts in that it uses a HashMap to
     * store both vertices and adjacency lists.
     *
     * @see https://github.com/sidhartha11/scalastudy
     */

    /** RECURSIVE VERSION **/

    // @tailrec

    private def dfsRecursive(graph: Graph[T], v: T): Unit = {
      marked(v) = true
      /** count each connected vertex **/
      idV(v) = counter
      /** get the adjacent neighbors of v **/
      for (w <- graph.adjreverse(v)) {
        /** if this neigbor has not been seen yet **/
        if (!hasPathTo(w)) {
          /** check the adjacent neighors of w **/
          dfsRecursive(graph, w)
        }
      }
    }

    /** ITERATIVE VERSION **/
    private def dfs(graph: Graph[T], v: T): Unit = {
      val stack = mutable.Stack[T]()
      /** push the first element onto the stack **/
      stack.push(v)

      while (!stack.isEmpty) {
        val s = stack.pop()
        if (!hasPathTo(s)) {
       
          marked(s) = true
          idV(s) = counter
          /** get all the adjacent vertices of s **/
          /** push each one that is not marked onto the stack **/
          for (w <- graph.adj(s)) {
            if (!hasPathTo(w)) {
             // idV(w) = counter
              stack.push(w)
            }
          }
        }
      }
    }

    override def toString = "KosarajuSharirSCC:" + counter

    /**
     *          public api implementation
     */

    /**
     * @return the number of verticies connect to input
     * source vertex
     */
    def count: Int = counter

    def hasPathTo(v: T): Boolean = {
      val t = marked get v
      if (t == None) false
      else t get
    }
    def stronglyConnected(v: T, w: T): Boolean = {
      id(v) == id(w)
    }

    def id(v: T): Option[Int] = {
      idV get v
    }
    def process(recurType: GraphConstants.Value): Unit = {
      /** disallow input of unknown vetex **/
      /** map used to determine is a vertex is connected to the input vertex,s **/
      println("recurType:" + recurType)
      marked = new mutable.HashMap[T, Boolean]()
      idV    = new mutable.HashMap[T,Int]()

      /** count of all vertices connected to input vertex s **/
      counter = 0
      /** first create a DepthFirstOrder Object **/
      /** pass it a reverse of the input adjacency graph **/
      val order = DepthFirstOrder(graph.reverseGraph)
      order.process(recurType)
      var fun: (Graph[T],T) => Unit = null
      recurType match {
        case GraphConstants.recursive     => fun = dfsRecursive _
        case GraphConstants.non_recursive => fun = dfs _
      }
    /**
     * Do a depth first search on the reversePost traversal of 
     * the DepthFirstOrder graph object, passing in the pre-reversed
     * graph object. 
     * 
     */
    for (k <- order.reversePostorder) {
      if ( ! hasPathTo(k) ) {
        fun(graph,k)
        counter += 1
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